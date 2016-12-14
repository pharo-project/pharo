logging-Deprecated
log: aLogStatement

	queue nextPut: aLogStatement.
	logging ifFalse: [
		logging := true.
		[ [ self basicLog: queue next ] repeat ] fork ].