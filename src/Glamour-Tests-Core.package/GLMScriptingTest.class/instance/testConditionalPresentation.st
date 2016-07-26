tests
testConditionalPresentation
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | 
					a text when: [ :x | x = 42 ].
					a list when: [ :x | x > 42 ] ].
	((browser paneNamed: #one) port: #selection) value: 42.
	self assert: (browser paneNamed: #two) matchingPresentations size equals: 1.
	self assert: ((browser paneNamed: #two) matchingPresentations first isKindOf: GLMRubricTextPresentation).
	((browser paneNamed: #one) port: #selection) value: 43.
	self assert: (browser paneNamed: #two) matchingPresentations size equals: 1.
	self assert: ((browser paneNamed: #two) matchingPresentations first isKindOf: GLMListPresentation)