accessing
structureAlignment
	externalStructureAlignment ifNil: [ self compileFields ].
	^ externalStructureAlignment