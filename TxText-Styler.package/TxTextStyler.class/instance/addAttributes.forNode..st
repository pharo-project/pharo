visiting
addAttributes: style forNode: node
	| start selection |
	start := text startPosition moveRight: node start - 1 .
	selection := start selectTo: (start moveRight: (node stop - node start + 1)).
	style attributes valuesDo: [ :each |	selection applyAttribute:  each ].