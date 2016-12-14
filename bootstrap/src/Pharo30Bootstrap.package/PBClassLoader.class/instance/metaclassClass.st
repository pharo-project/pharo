stubs
metaclassClass
	
	"Base case for recursion on class creation.
	Ok, so here we really go down and create the metaclass from nothing. We will use this metaclass to create all the others"
	| aClassMapping theNewMetaclass theNewMetaclassClass classFormat metaclassFormat |
	
	environment at: #Metaclass ifPresent: [ :aClass | ^ aClass ].

	aClassMapping := systemDefinition classNamed: #Metaclass.
	classFormat := self formatOfClass: aClassMapping.	
	metaclassFormat := self formatOfClass: aClassMapping classSide.
	
	theNewMetaclass := builder objectSpace
		createClassWithFormat: metaclassFormat
		forInstancesOfFormat: classFormat.
	
	theNewMetaclassClass := builder objectSpace
		createClassWithFormat: classFormat
		forInstancesOfFormat: metaclassFormat.
	
	theNewMetaclass setClass: theNewMetaclassClass.
	theNewMetaclassClass setClass: theNewMetaclass.
	
	self registerClass: theNewMetaclass named: aClassMapping name.
	^ theNewMetaclass