tests
testSimpleOpen
	| p |
	p := GLMCompositePresentation new.
	p tabulator with: [:b | b row: #one. b transmit to: #one; andShow: [:a | a text ] ].
	window := p openOn: 42.
	self assert: (window isKindOf: GLMSystemWindow)