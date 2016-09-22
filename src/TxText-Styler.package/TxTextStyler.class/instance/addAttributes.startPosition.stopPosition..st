attributes
addAttributes: style startPosition: startPosition stopPosition: stopPosition
	| start selection |
	start := text startPosition moveRight: startPosition - 1.
	selection := start selectTo: (start moveRight: stopPosition - startPosition + 1).
	style attributes valuesDo: [ :each | selection applyAttribute: each ]