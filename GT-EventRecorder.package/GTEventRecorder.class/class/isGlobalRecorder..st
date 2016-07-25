testing
isGlobalRecorder: aGTEventRecorder
	^ uniqueInstance 
			ifNil: false
			ifNotNil: [ :anInstance | anInstance == aGTEventRecorder  ]