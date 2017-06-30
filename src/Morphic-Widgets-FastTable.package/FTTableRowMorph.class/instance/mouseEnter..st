event handling
mouseEnter: evt
	
	over ifFalse: [ 
		over := true.
		self changed	
	]