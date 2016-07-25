accessing
basicInspector: anEyeInspector
	inspector value 
		ifNotNil: [ :inspect | 
			inspect owner: nil ].
	inspector value: anEyeInspector.
	
	anEyeInspector 
		owner: self;
		ownerChanged.