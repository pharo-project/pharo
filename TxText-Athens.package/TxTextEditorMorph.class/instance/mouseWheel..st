event handling
mouseWheel: event
	event isUp ifTrue: [
		layoutViewMorph scrollBy: 0@ (layoutViewMorph height/10) negated.
		  ].
	event isDown ifTrue: [ 
		layoutViewMorph scrollBy: 0@ (layoutViewMorph height/10).
	  ].
	
	self updateCursor: false.