private
itemFilterNameFor: anObject
	^ itemFilterNameBlock isNil
		ifTrue: [ super itemFilterNameFor: anObject ]
		ifFalse: [ itemFilterNameBlock cull: anObject ]