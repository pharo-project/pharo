accessing
removedMethods
	^removedMethods isNil 
		ifTrue: [removedMethods := Set new]
		ifFalse: [removedMethods]