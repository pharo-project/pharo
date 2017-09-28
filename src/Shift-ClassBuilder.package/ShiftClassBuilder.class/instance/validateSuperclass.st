private validating
validateSuperclass
	self superclass ifNil: [ ^self ].
	oldClass ifNil: [ ^ self ].	
		
	self superclass withAllSuperclassesDo: [ :aSuperclass |
		aSuperclass = oldClass ifTrue:[
			CircularHierarchyError signalFor: oldClass. 
		]
	].	
