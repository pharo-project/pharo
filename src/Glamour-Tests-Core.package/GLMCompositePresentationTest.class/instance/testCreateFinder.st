tests - composer
testCreateFinder
	| show |
	show := GLMCompositePresentation new.
	show finder with: [ :b | b show: [ :a | a text ] ].
	self assert: show size equals: 1.
	self assert: (show first isKindOf: GLMFinder).
	show startOn: 42.
	self assert: show first panes size equals: 1.
	self assert: (show first panes first port: #entity) value equals: 42