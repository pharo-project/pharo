brick-morph-mouse-events
mouseDown: evt
	|selectors|
	
	self isDoubleClickEnabled ifFalse: [
		super mouseDown: evt.
		^ self press ].
	
	selectors := Array
				with: #click:
				with: #doubleClick:
				with: #singleClick:
				with: nil.
				
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: selectors
		threshold: 10.
