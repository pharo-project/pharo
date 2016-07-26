event handling
mouseWheel: event

	"I tried scrolling up/down with a calculated value (check #scrollUpByPageDelta implementor)
	 but the scrollbar proved been more intelligent than me... looks like hardcoded values 
	 work better :/"
	event isUp ifTrue: [ 
		self verticalScrollBar scrollRestrictedUp: 3.
		^ self
	].
	event isDown ifTrue: [ 
		self verticalScrollBar scrollRestrictedDown: 3.
		^ self
	].
	
	super mouseWheel: event