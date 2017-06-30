event handling
mouseLeave: evt
	
	over ifTrue: [ 
		over := false.
		self changed	
	]
	 