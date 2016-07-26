comparing
requirements
	^ ((superclassName == #nil) or: [superclassName asString beginsWith: 'AnObsolete'])
		ifTrue: [self poolDictionaries]
		ifFalse: [(Array with: superclassName), self poolDictionaries]