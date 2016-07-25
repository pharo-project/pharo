tests
testAllowNil
	| browser detailsMorph |
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
	window := browser openOn: 42.
	((browser paneNamed: #one) port: #selection) value: 2.
	detailsMorph := window submorphs last submorphs last submorphs first.
	self assert: detailsMorph text asString equals: '2nil'.
	((browser paneNamed: #two) port: #selection) value: #c.
	detailsMorph := window submorphs last submorphs last submorphs first.
	self assert: detailsMorph text asString equals: '2c'.
	((browser paneNamed: #one) port: #selection) value: nil.
	detailsMorph := window submorphs last submorphs last submorphs first.
	self assert: detailsMorph text asString equals: 'nilc'