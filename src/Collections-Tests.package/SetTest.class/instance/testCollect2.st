tests - some
testCollect2
	| newFull result |
	newFull := self classToBeTested withAll: (1 to: 10).
	result := newFull
		collect: [ :each | 
			each >= 1
				ifTrue: [ each ]
				ifFalse: [ 'no' ] ].
	self assert: result equals: newFull.
	result := newFull
		collect: [ :each | 
			each >= 5
				ifTrue: [ each ]
				ifFalse: [ 'no' ] ].
	self assert: result equals: ((Set withAll: (5 to: 10)) add: 'no'; yourself)