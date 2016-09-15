accessing
targetsOf: anObject

	^ (self conditionBlock cull: anObject) 
		ifTrue: [ self targetsBlock cull: anObject ]
		ifFalse: [ #() ]