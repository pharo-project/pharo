accessing
silent
	
	^ silent ifNil: [ self beVerbose. false ]