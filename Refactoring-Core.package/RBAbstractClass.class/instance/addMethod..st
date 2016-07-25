method accessing
addMethod: aRBMethod 
	self newMethods at: aRBMethod selector put: aRBMethod.
	removedMethods notNil 
		ifTrue: [removedMethods remove: aRBMethod selector ifAbsent: []]