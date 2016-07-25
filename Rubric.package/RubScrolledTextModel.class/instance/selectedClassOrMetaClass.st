accessing
selectedClassOrMetaClass
	^ interactionModel ifNotNil: [ :im | im selectedClassOrMetaClass ]