accessing
classForClassMirror: anEPClassMirror 
	
	^ mirrorDefinitionMap at: anEPClassMirror ifAbsent: [ systemDefinition classNamed: #Class ]