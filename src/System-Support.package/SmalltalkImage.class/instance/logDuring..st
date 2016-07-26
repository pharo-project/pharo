miscellaneous
logDuring: aMonadicBlock
	" for safe use, if stream is a file, it needs to be closed after use " 
	| logStream |	

	[
		logStream := self openLog.
		aMonadicBlock value: logStream.
	] ensure: [ self closeLog: logStream ].

