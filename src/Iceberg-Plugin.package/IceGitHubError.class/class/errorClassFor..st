private
errorClassFor: aResponse
	^ self allSubclasses 
		detect: [ :each | each isAppropriateFor: aResponse  ]
		ifNone: [ self ]