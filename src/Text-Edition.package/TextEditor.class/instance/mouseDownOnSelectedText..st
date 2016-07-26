events
mouseDownOnSelectedText: event
	| selectors |
	selectors := Array
		with: #mouseDownOnUnselectedText:
		with: nil
		with: nil
		with:
			(self morph dragEnabled
				ifTrue: [ #startDrag: ]
				ifFalse: [ nil ]).
	event hand
		waitForClicksOrDrag: self
		event: event
		selectors: selectors
		threshold: 10