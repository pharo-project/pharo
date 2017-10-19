tests - controlling
testIfTrue
	self
		assert:
			(false
				ifTrue: [ 'alternativeBlock' ]) isNil