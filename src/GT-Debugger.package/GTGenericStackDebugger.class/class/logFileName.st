settings api
logFileName
	^ LogFileName ifNil: [ self defaultLogFileName ] 