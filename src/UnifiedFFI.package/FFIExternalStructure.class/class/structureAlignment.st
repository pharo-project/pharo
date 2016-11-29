accessing
structureAlignment
	externalStructureAlignment ifNil: [ self compileFields: self fields withAccessors: false ].
	^ externalStructureAlignment