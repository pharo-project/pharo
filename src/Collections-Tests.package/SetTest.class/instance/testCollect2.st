tests - some
testCollect2
	| newFull result |
	newFull := Set withAll: (1 to: 10).
	result := newFull collect: [:each | each >= 1 ifTrue: [each] ifFalse: ['no']].
	self assert: (result = newFull).
	result := newFull collect: [:each | each >= 5 ifTrue: [each] ifFalse: ['no']].
	self assert: (result = ((Set withAll: (5 to: 10)) add: 'no'; yourself)).