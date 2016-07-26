handling
defaultAction
	Log 
		ifNotNil: [:log | log add: self].
	self logTranscript.
	self raiseWarning 
		ifTrue: [super defaultAction]