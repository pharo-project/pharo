testing
isUserInterrupt

	^ UserInterruptHandler cmdDotEnabled and: [ self keyCharacter = $. and: [ self commandKeyPressed ] ].
	