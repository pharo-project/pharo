tests
testAllowNil
	| browser presentationsChanged matchingPresentationsChanged |
	browser := GLMTabulator new.
	browser
		column: [ :c | 
					c
						row: #one;
						row: #two ];
		column: #details.
	browser
		transmit to: #one;
		andShow: [ :a | a tree display: #(1 2 3) ].
	browser
		transmit to: #two;
		andShow: [ :a | a tree display: #(#a #b #c) ].
	browser
		transmit to: #details;
		from: #one;
		from: #two;
		andShow: [ :a | 
					(a text)
						display: [ :one :two | one asString , two asString ];
						allowNil ].
	browser startOn: 42.
	(browser paneNamed: #details) when: GLMPresentationsChanged do: [ :a | presentationsChanged := true ].
	(browser paneNamed: #details) when: GLMMatchingPresentationsChanged do: [ :a | matchingPresentationsChanged := true ].
	presentationsChanged := false.
	matchingPresentationsChanged := false.
	((browser paneNamed: #one) port: #selection) value: 2.
	self assert: presentationsChanged.
	self assert: matchingPresentationsChanged.
	self assert: (browser paneNamed: #details) presentations first displayValue equals: '2nil'.
	presentationsChanged := false.
	((browser paneNamed: #two) port: #selection) value: #c.
	self assert: presentationsChanged.
	self assert: matchingPresentationsChanged.
	self assert: (browser paneNamed: #details) presentations first displayValue equals: '2c'.
	presentationsChanged := false.
	((browser paneNamed: #one) port: #selection) value: nil.
	self assert: presentationsChanged.
	self assert: matchingPresentationsChanged.
	self assert: (browser paneNamed: #details) presentations first displayValue equals: 'nilc'