initialization
finalizeFeedback: aBoolean withComment: aString

	text contents: 'Thanks!'.
	self
		removeMorph: positive;
		removeMorph: negative.
	
	[ aBoolean
		ifTrue:  [ critique providePositiveFeedback: aString ]
		ifFalse: [ critique provideNegativeFeedback: aString ]
	] fork.

	[ 2 seconds asDelay wait.
	  self delete ] fork.