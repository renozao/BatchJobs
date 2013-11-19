############################################
### Common database functions
############################################
sqlQuote = function(x) {
  sprintf("'%s'", x)
}

dbGetConnection = function(drv, ...) {
  # method dispatch tp support different DBMS
  UseMethod("dbGetConnection")
}

dbGetConnection.SQLiteDriver = function(drv, reg, flags="ro", ...) {
  flags = switch(flags, "ro" = SQLITE_RO, "rw" = SQLITE_RW, "rwc" = SQLITE_RWC)
  opts = list(dbname = file.path(reg$file.dir, "BatchJobs.db"),
              flags = flags, drv = drv)
  do.call(dbConnect, args=c(reg$db.options, opts))
}

dbConnectToJobsDB = function(reg, flags="ro") {
  drv = do.call(reg$db.driver, list())
  dbGetConnection(drv, reg, flags)
}

dbDoQueries = function(reg, queries, flags="ro", max.retries=200L, sleep=function(r) 1.025^r) {
  for (i in seq_len(max.retries)) {
    con = dbConnectToJobsDB(reg, flags)
    ok = try ({
      dbBeginTransaction(con)
      ress = lapply(queries, dbGetQuery, con=con)
    }, silent = TRUE)
    if (!is.error(ok)) {
      # this can fail because DB is locked
      ok2 = dbCommit(con)
      if (ok2) {
        dbDisconnect(con)
        return(ress)
      } else {
        dbRollback(con)
        dbDisconnect(con)
      }
    } else {
      ok = as.character(ok)
      dbRollback(con)
      dbDisconnect(con)
      # catch known temporary errors:
      # - database is still locked
      # - disk I/O error
      if(!grepl("(lock|i/o)", tolower(ok)))
        stopf("Error in dbDoQueries. Displaying only 1st query. %s (%s)", ok, queries[1L])
    }
    # if we reach this here, DB was locked or temporary I/O error
    Sys.sleep(runif(1L, min=1, max=sleep(i)))
  }
  stopf("dbDoQueries: max retries (%i) reached, database is still locked!", max.retries)
}

dbDoQuery = function(reg, query, flags="ro", max.retries=200L, sleep=function(r) 1.025^r) {
  for (i in seq_len(max.retries)) {
    con = dbConnectToJobsDB(reg, flags)
    res = try(dbGetQuery(con, query), silent=TRUE)
    dbDisconnect(con)
    if (! is.error(res))
      return(res)
    res = as.character(res)
    if(grepl("(lock|i/o)", tolower(res))) {
      Sys.sleep(runif(1L, min=1, max=sleep(i)))
    } else {
      print(res)
      print(query)
      stopf("Error in dbDoQuery. %s (%s)", res, query)
    }
  }
  stopf("dbDoQuery: max retries (%i) reached, database is still locked!", max.retries)
}


dbAddData = function(reg, tab, data) {
  query = sprintf("INSERT INTO %s_%s (%s) VALUES(%s)", reg$id, tab,
                  collapse(colnames(data)), collapse(rep.int("?", ncol(data))))
  con = dbConnectToJobsDB(reg, flags="rw")
  on.exit(dbDisconnect(con))
  dbBeginTransaction(con)
  ok = try(dbGetPreparedQuery(con, query, bind.data = data))
  if(is.error(ok)) {
    dbRollback(con)
    stopf("Error in dbAddData: %s", as.character(ok))
  }

  dbCommit(con)
  as.integer(dbGetQuery(con, "SELECT total_changes()"))
}

dbSelectWithIds = function(reg, query, ids, where=TRUE, group.by, limit, reorder=TRUE) {
  if(!missing(ids))
    query = sprintf("%s %s job_id IN (%s)", query, ifelse(where, "WHERE", "AND"), collapse(ids))
  if(!missing(group.by))
    query = sprintf("%s GROUP BY %s", query, collapse(group.by))
  if(!missing(limit))
    query = sprintf("%s LIMIT %i", query, limit)

  res = dbDoQuery(reg, query)
  if(missing(ids) || !reorder)
    return(res)
  return(res[na.omit(match(ids, res$job_id)),, drop=FALSE])
}

############################################
### CREATE
############################################
#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return Nothing.
#' @export
dbCreateJobDefTable = function(reg) {
  UseMethod("dbCreateJobDefTable")
}

#' @S3method dbCreateJobDefTable Registry
dbCreateJobDefTable.Registry = function(reg) {
  #message("Initializing job definition table...")

  query = sprintf("CREATE TABLE %s_job_def (job_def_id INTEGER PRIMARY KEY, fun_id TEXT, pars TEXT, jobname TEXT)", reg$id)
  dbDoQuery(reg, query, flags="rwc")
  dbCreateExpandedJobsView(reg)
}

dbCreateJobStatusTable = function(reg, extra.cols="", constraints="") {
  #message("Initializing job status table...")

  query = sprintf(paste("CREATE TABLE %s_job_status (job_id INTEGER PRIMARY KEY, job_def_id INTEGER,",
                   "first_job_in_chunk_id INTEGER, seed INTEGER, resources_timestamp INTEGER, submitted INTEGER,",
                   "started INTEGER, batch_job_id TEXT, node TEXT, r_pid INTEGER,",
                   "done INTEGER, error TEXT %s %s)"), reg$id, extra.cols, constraints)
  dbDoQuery(reg, query, flags="rwc")

  query = sprintf("CREATE INDEX job_def_id ON %s_job_status(job_def_id)", reg$id)
  dbDoQuery(reg, query, flags="rw")

  return(invisible(TRUE))
}

dbCreateExpandedJobsView = function(reg) {
  query = sprintf("CREATE VIEW %1$s_expanded_jobs AS SELECT * FROM %1$s_job_status LEFT JOIN %1$s_job_def USING(job_def_id)", reg$id)
  dbDoQuery(reg, query, flags="rw")
}


############################################
### SELECT
############################################

#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected jobs.
#' @return [list of \code{\link{Job}}]. Retrieved jobs from DB.
#' @export
dbGetJobs = function(reg, ids) {
  UseMethod("dbGetJobs")
}

# note that this does not load the job function from disk to increase speed
#' @method dbGetJobs Registry
#' @S3method dbGetJobs Registry
dbGetJobs.Registry = function(reg, ids) {
  query = sprintf("SELECT job_id, fun_id, pars, jobname, seed FROM %s_expanded_jobs", reg$id)
  tab = dbSelectWithIds(reg, query, ids)
   lapply(seq_row(tab), function(i) {
    makeJob(id=tab$job_id[i],
            fun.id=tab$fun_id[i],
            fun=NULL,
            pars=unserialize(charToRaw(tab$pars[i])),
            name=tab$jobname[i],
            seed=tab$seed[i])
  })
}

dbGetExpandedJobsTable = function(reg, ids, cols="*") {
  # Note: job_id must be in cols!
  query = sprintf("SELECT %s FROM %s_expanded_jobs", collapse(cols), reg$id)
  tab = dbSelectWithIds(reg, query, ids)
  setRowNames(tab, tab$job_id)
}

dbGetJobStatusTable = function(reg, ids, cols="*") {
  # Note: job_id must be in cols!
  query = sprintf("SELECT %s FROM %s_job_status", collapse(cols), reg$id)
  tab = dbSelectWithIds(reg, query, ids)
  setRowNames(tab, tab$job_id)
}

dbGetJobCount = function(reg) {
  query = sprintf("SELECT COUNT(*) AS count FROM %s_job_status", reg$id)
  dbDoQuery(reg, query)$count
}

dbGetJobId = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status LIMIT 1", reg$id)
  as.integer(dbDoQuery(reg, query)$job_id)
}

dbGetJobIds = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbCheckJobIds = function(reg, ids) {
  not.found = setdiff(ids, dbGetJobIds(reg))
  if (length(not.found) > 0L)
    stopf("Ids not present in registry: %s", collapse(not.found))
}

dbGetJobIdsIfAllDone = function(reg) {
  query = sprintf("SELECT job_id, done FROM %s_job_status", reg$id)
  res = dbDoQuery(reg, query)
  if (all(! is.na(res$done)))
      return(res$job_id)
  stop("Not all jobs finished (yet)!")
}

dbGetLastAddedIds = function(reg, tab, id.col, n) {
  query = sprintf("SELECT %s AS id_col FROM %s_%s ORDER BY %s DESC LIMIT %i",
                  id.col, reg$id, tab, id.col, n)
  rev(dbDoQuery(reg, query)$id_col)
}

dbFindDone = function(reg, ids, negate=FALSE) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (done IS NOT NULL)", reg$id, if(negate) "NOT" else "")
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindErrors = function(reg, ids, negate=FALSE) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (error IS NOT NULL)", reg$id, if(negate) "NOT" else "")
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindTerminated = function(reg, ids, negate=FALSE) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (done IS NOT NULL OR error IS NOT NULL)", reg$id, if(negate) "NOT" else "")
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindSubmitted = function(reg, ids, negate=FALSE) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (submitted IS NOT NULL)", reg$id, if (negate) "NOT" else "")
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindStarted = function(reg, ids, negate=FALSE) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (started IS NOT NULL)", reg$id, if (negate) "NOT" else "")
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindOnSystem = function(reg, ids, negate=FALSE, batch.ids) {
  if (missing(batch.ids))
    batch.ids = getBatchIds(reg, "Cannot find jobs on system")

  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (batch_job_id IN (%s))",
                  reg$id, if (negate) "NOT" else "", collapse(sqlQuote(batch.ids)))
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindRunning = function(reg, ids, negate=FALSE, batch.ids) {
  if (missing(batch.ids))
    batch.ids = getBatchIds(reg, "Cannot find jobs on system")

  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (batch_job_id IN (%s) AND started IS NOT NULL AND done IS NULL AND error IS NULL)",
                  reg$id, if (negate) "NOT" else "", collapse(sqlQuote(batch.ids)))
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbFindExpiredJobs = function(reg, ids, negate=FALSE, batch.ids) {
  if (missing(batch.ids))
    batch.ids = getBatchIds(reg, "Cannot find jobs on system")
  # started, not terminated, not running
  query = sprintf("SELECT job_id FROM %s_job_status WHERE %s (started IS NOT NULL AND done IS NULL AND error is NULL AND
                  batch_job_id NOT IN (%s))", reg$id, if (negate) "NOT" else "", collapse(sqlQuote(batch.ids)))
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}


dbGetFirstJobInChunkIds = function(reg, ids){
  query = sprintf("SELECT job_id, first_job_in_chunk_id FROM %s_job_status", reg$id)
  dbSelectWithIds(reg, query, ids)$first_job_in_chunk_id
}

dbAnyErrors = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE error IS NOT NULL", reg$id)
  as.logical(nrow(dbSelectWithIds(reg, query, ids, where=FALSE, reorder=FALSE, limit=1L)))
}

dbGetErrorMsgs = function(reg, ids, filter=FALSE, limit) {
  query = sprintf("SELECT job_id, error from %s_job_status", reg$id)
  if (filter)
    query = sprintf("%s WHERE error IS NOT NULL", query)
  dbSelectWithIds(reg, query, ids, where=!filter, limit=limit)
}

dbGetStats = function(reg, ids, running=FALSE, expired=FALSE, times=FALSE, batch.ids) {
  cols = c(n         = "COUNT(job_id)",
           submitted = "COUNT(submitted)",
           started   = "COUNT(started)",
           done      = "COUNT(done)",
           error     = "COUNT(error)",
           running   = "NULL",
           expired   = "NULL",
           t_min     = "NULL",
           t_avg     = "NULL",
           t_max     = "NULL")

  if (missing(batch.ids) && (expired || running))
    batch.ids = getBatchIds(reg, "Cannot find jobs on system")
  if(running)
    cols["running"] = sprintf("SUM(started IS NOT NULL AND done IS NULL AND error IS NULL AND batch_job_id IN (%s))", collapse(sqlQuote(batch.ids)))
  if(expired)
    cols["expired"] = sprintf("SUM(started IS NOT NULL AND done IS NULL AND error IS NULL AND batch_job_id NOT IN (%s))", collapse(sqlQuote(batch.ids)))
  if (times)
    cols[c("t_min", "t_avg", "t_max")] = c("MIN(done - started)", "AVG(done - started)", "MAX(done - started)")

  query = sprintf("SELECT %s FROM %s_job_status", collapse(paste(cols, "AS", names(cols)), sep = ", "), reg$id)
  df = dbSelectWithIds(reg, query, ids, reorder=FALSE)

  # Convert to correct type. Null has no type and casts tend to not work properly with RSQLite
  x = c("n", "submitted", "started", "done", "error", "running", "expired")
  df[x] = lapply(df[x], as.integer)
  x = c("t_min", "t_avg", "t_max")
  df[x] = lapply(df[x], as.double)
  df
}

dbGetJobNames = function(reg, ids) {
  query = sprintf("SELECT job_id, jobname FROM %s_expanded_jobs", reg$id)
  as.character(dbSelectWithIds(reg, query, ids)$jobname)
}

dbMatchJobNames = function(reg, ids, jobnames) {
  query = sprintf("SELECT job_id FROM %s_expanded_jobs WHERE jobname IN (%s)", reg$id, collapse(sqlQuote(jobnames)))
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

############################################
### DELETE
############################################
dbRemoveJobs = function(reg, ids) {
  query = sprintf("DELETE FROM %s_job_status WHERE job_id IN (%s)", reg$id, collapse(ids))
  dbDoQuery(reg, query, flags="rw")
  query = sprintf("DELETE FROM %1$s_job_def WHERE job_def_id NOT IN (SELECT DISTINCT job_def_id FROM %1$s_job_status)", reg$id)
  dbDoQuery(reg, query, flags="rw")
  return(invisible(TRUE))
}


############################################
### Messages
############################################
dbSendMessage = function(reg, msg, staged = FALSE) {
  if (staged) {
    fn = getSQLFileName(reg, msg$type, msg$ids[1L], getOrderCharacters()[msg$type])
    writeSQLFile(msg$msg, fn)
  } else {
    dbDoQuery(reg, msg$msg, flags="rw")
  }
}

dbSendMessages = function(reg, msgs, max.retries=200L, sleep=function(r) 1.025^r, staged = FALSE) {
  if (length(msgs) == 0L)
    return(TRUE)

  if (staged) {
    chars = getOrderCharacters()

    # reorder messages in sublist
    msgs = split(msgs, extractSubList(msgs, "type"))
    msgs = msgs[order(match(names(msgs), names(chars)))]

    for (cur in msgs) {
      first = cur[[1L]]
      fn = getSQLFileName(reg, first$type, first$ids[1L], chars[first$type])
      writeSQLFile(extractSubList(cur, "msg"), fn)
    }
  } else {
    ok = try(dbDoQueries(reg, extractSubList(msgs, "msg"), flags="rw", max.retries, sleep))
    if (is.error(ok)) {
      ok = as.character(ok)
      if (ok == "dbDoQueries: max retries reached, database is still locked!") {
        return(FALSE)
      } else {
        #throw exception again
        stopf("Error in dbSendMessages: %s", ok)
      }
    }
  }

  return(TRUE)
}


dbMakeMessageSubmitted = function(reg, job.ids, time=now(), batch.job.id, first.job.in.chunk.id=NULL, resources.timestamp) {
  if(is.null(first.job.in.chunk.id))
    first.job.in.chunk.id = "NULL"
  updates = sprintf("first_job_in_chunk_id=%s, submitted=%i, batch_job_id='%s', resources_timestamp=%i",
                    first.job.in.chunk.id, time, batch.job.id, resources.timestamp)
  list(msg = sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids)),
       ids = job.ids,
       type = "submitted")
}

dbMakeMessageStarted = function(reg, job.ids, time=now()) {
  node = gsub("'", "\"", Sys.info()["nodename"], fixed=TRUE)
  updates = sprintf("started=%i, node='%s', r_pid=%i, error=NULL, done=NULL", time, node, Sys.getpid())
  list(msg = sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids)),
       ids = job.ids,
       type = "started")
}

dbMakeMessageError = function(reg, job.ids, err.msg) {
  # FIXME how to escape ticks (')? Just replaced with double quotes for the moment
  err.msg = gsub("'", "\"", err.msg, fixed=TRUE)
  err.msg = gsub("[^[:print:]]", " ", err.msg)
  updates = sprintf("error='%s', done=NULL", err.msg)
  list(msg = sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids)),
       ids = job.ids,
       type = "error")
}

dbMakeMessageDone = function(reg, job.ids, time=now()) {
  updates = sprintf("done=%i, error=NULL", time)
  list(msg = sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids)),
       ids = job.ids,
       type = "done")
}

dbMakeMessageKilled = function(reg, job.ids) {
  updates = "resources_timestamp=NULL, submitted=NULL, started=NULL, batch_job_id=NULL, node=NULL, r_pid=NULL, done=NULL, error=NULL"
  list(msgs = sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids)),
       ids = job.ids,
       type = "killed")
}

dbConvertNumericToPOSIXct = function(x) {
  now = Sys.time()
  as.POSIXct(x, origin=now - as.integer(now))
}

dbSetJobFunction = function(reg, ids, fun.id) {
  query = sprintf("UPDATE %1$s_job_def SET fun_id = '%2$s' WHERE job_def_id IN (SELECT job_def_id FROM %1$s_job_status WHERE job_id IN (%3$s))", reg$id, fun.id, collapse(ids))
  dbDoQuery(reg, query, flags="rw")
}

dbSetJobNames = function(reg, ids, jobnames) {
  queries = sprintf("UPDATE %1$s_job_def SET jobname = '%2$s' WHERE job_def_id IN (SELECT job_def_id FROM %1$s_job_status WHERE job_id IN (%3$i))", reg$id, jobnames, ids)
  dbDoQueries(reg, queries, flags="rw")
}