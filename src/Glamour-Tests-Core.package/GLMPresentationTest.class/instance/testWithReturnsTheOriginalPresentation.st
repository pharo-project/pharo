tests
testWithReturnsTheOriginalPresentation
	| p result |
	p := GLMPresentation new.
	result := p with: [ :thePresentation | GLMPresentation new ].
	self assert: result == p