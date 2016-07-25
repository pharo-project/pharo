copying
rootPrototype
	^ self parentPrototype isNil 
		ifTrue: [ self ] 
		ifFalse: [ self parentPrototype rootPrototype ]
