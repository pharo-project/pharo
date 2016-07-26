tests
testWithBlockPassesTheParentPresentation
	| p presentationInWithBlock |
	p := GLMPresentation new.
	p with: [ :thePresentation | presentationInWithBlock := thePresentation ].
	self assert: presentationInWithBlock == p