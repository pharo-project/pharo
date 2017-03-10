scheduling
schedule
	lastSchedule := DateAndTime now.

	process ifNil: [ 
		"Deferrer can be activated during tests which intercept any forked processes.
		To prevent it we should ensure here that message process is running out of tests environment"
		DefaultExecutionEnvironment beActiveDuring: [ self runMessageProcess ]]