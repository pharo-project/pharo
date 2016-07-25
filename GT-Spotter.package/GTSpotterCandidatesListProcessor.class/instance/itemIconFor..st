private
itemIconFor: anObject
	^ itemIconBlock isNil
		ifTrue: [ super itemIconFor: anObject ]
		ifFalse: [ itemIconBlock cull: anObject ]