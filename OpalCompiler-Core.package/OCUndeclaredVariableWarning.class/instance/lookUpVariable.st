correcting
lookUpVariable
	^ (node owningScope lookupVar: node name)
		ifNil: [self error: 'should be found']