private
itemNameFor: anObject
	^ itemNameBlock isNil
		ifTrue: [ super itemNameFor: anObject ]
		ifFalse: [ itemNameBlock cull: anObject ]