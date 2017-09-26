finalization
finalizeValues
	
	| head |
	
	head := self finalizationList swapWithNil.
	
	[ head notNil ] whileTrue: [ 
		head finalize.
		head := head next
	].