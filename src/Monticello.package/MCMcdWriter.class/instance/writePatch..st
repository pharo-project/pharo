writing
writePatch: aPatch
	| old new |
	old := OrderedCollection new.
	new := OrderedCollection new.
	aPatch operations do:
		[:ea |
		ea isRemoval ifTrue: [old add: ea definition].
		ea isAddition ifTrue: [new add: ea definition].
		ea isModification ifTrue: [old add: ea baseDefinition. new add: ea definition]].
	self writeOldDefinitions: old.
	self writeNewDefinitions: new.
	self addString: (self serializeInBinary: aPatch) at: 'patch.bin'.