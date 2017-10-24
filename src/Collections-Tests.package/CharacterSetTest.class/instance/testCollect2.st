some tests
testCollect2
	| newFull result |
	newFull := self classToBeTested withAll: ($A to: $J).
	result := newFull
		collect: [ :each | 
			each >= $A
				ifTrue: [ each ]
				ifFalse: [ $X ] ].
	self assert: result equals: newFull.
	result := newFull
		collect: [ :each | 
			each >= $E
				ifTrue: [ each ]
				ifFalse: [ $X ] ].
	self assert: result equals: ((Set withAll: ($E to: $J)) add: $X; yourself)