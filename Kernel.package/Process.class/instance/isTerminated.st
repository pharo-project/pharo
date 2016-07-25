testing
isTerminated
	self isActiveProcess ifTrue: [ ^ false ].
	
	^ suspendedContext isNil or: [
		"If the suspendedContext is the bottomContext it is the block in Process>>newProcess.
		If so, and the pc is greater than the startpc, the block has already sent and returned
		from value and there is nothing more to do."
		suspendedContext isBottomContext and: [
			suspendedContext isDead or: [
				"The pc of the suspendedContext is set to nil in #terminate explicitly.
				Leaving this line in for safety."
  	  			suspendedContext pc > suspendedContext startpc ] ] ]
