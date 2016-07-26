surface management
checkSurface

	session == Smalltalk session ifFalse: [ 
		self initializeForNewSession
		 ].
	
	bounds extent ~= surface extent ifTrue: [ 
		"recreate a surface if dimensions are changed"	
		self createSurface
	 ]
