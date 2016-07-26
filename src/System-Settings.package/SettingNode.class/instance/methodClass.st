accessing
methodClass
	^ self pragma ifNotNil: [self pragma methodClass theNonMetaClass]