tests - composer
testCreateTabulator
	| show |
	show := GLMCompositePresentation new.
	show tabulator
		with: [ :b | 
			b row: #one.
			b transmit
				to: #one;
				transformed: [ :x | x + 1 ];
				andShow: [ :a | a text ] ].
	self assert: show size equals: 1.
	self assert: (show first isKindOf: GLMTabulator).
	show startOn: 41.
	self assert: show first panes size equals: 1.
	self assert: (show first panes first port: #entity) value equals: 42