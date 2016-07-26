method tests
testHierarchy
	| meta |
	meta := objectClass theMetaClass.
	self assert: (objectClass withAllSubclasses includes: meta).
	self assert: (meta withAllSuperclasses includes: objectClass)