morphic - events
mouseDown: evt
	|selectors|
	
	self isEnabled ifFalse: [ ^ self ].
	
	selectors := Array
				with: #singleClick:
				with: #doubleClick:
				with: nil
				with: nil.
				
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: selectors
		threshold: 10
	