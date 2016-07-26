testing
isGlobalRecorder
	^ self recorder 
			ifNil: false
			ifNotNil: #isGlobalRecorder