running
tearDown 
	debugger ifNotNil: [ debugger close].
	window := nil.
	debugger := nil.
	session := nil.