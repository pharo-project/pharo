as yet unclassified
metaclassClass
	
	"Base case for recursion on class creation.
	Ok, so here we really go down and create the metaclass from nothing. We will use this metaclass to create all the others"
	| aClassMapping theNewMetaclass theNewMetaclassClass classFormat metaclassFormat metaclassIndex classIndex |
	
	environment at: #Metaclass ifPresent: [ :aClass | ^ aClass ].
	aClassMapping := systemDefinition classNamed: #Metaclass.
	classFormat := self formatOfClass: aClassMapping.	
	metaclassFormat := self formatOfClass: aClassMapping classSide.
	
	metaclassIndex := self newClassIndex.
	classIndex := self newClassIndex.
	
	theNewMetaclass := (builder objectSpace backend createObjectWithFormat: metaclassFormat withSize: 0 classIndex: classIndex) asClassMirror.
	theNewMetaclass format: classFormat.
	builder objectSpace backend objectMemory setHashBitsOf: theNewMetaclass target address to: metaclassIndex.
	builder objectSpace backend objectMemory classAtIndex: metaclassIndex put: theNewMetaclass target address.	
	
	theNewMetaclassClass := (builder objectSpace backend createObjectWithFormat: classFormat withSize: 0 classIndex: metaclassIndex) asClassMirror.
	theNewMetaclassClass format: metaclassFormat.
	builder objectSpace backend objectMemory setHashBitsOf: theNewMetaclassClass target address to: classIndex.
	builder objectSpace backend objectMemory classAtIndex: classIndex put: theNewMetaclassClass target address.

	self registerClass: theNewMetaclass named: aClassMapping name.
	^ theNewMetaclass