operation
deleteUpTo: end startingAt: start
	
	end span == self ifTrue: [ "nothing to remove"  ^ end ].
	
	self next: end span.
	end span removeBefore: end spanPosition.
	
	^ self tryMergeAndUpdatePosition: start 