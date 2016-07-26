tests
testInterdependentPanesCycleBreak
	| browser outerCount oneCount |
	outerCount := 0.
	oneCount := 0.
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser
		transmit to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x * 2 ] ].
	browser
		transmit to: #three;
		from: #two;
		andShow: [ :a |  
					(a list)
						display: [ :x | 1 to: x * 3 ];
						populate: #focus
							on: $f
							entitled: 'Focus'
							with: [ :list | list selection ] ].
	browser transmit toOutsidePort: #focus; from: #three port: #focus; transformed: [ :x | outerCount := outerCount + 1 ].
	browser transmit to: #one port: #selection; fromOutsidePort: #focus; transformed: [ :x | oneCount := oneCount + 1 ].
	browser startOn: 5.
	((browser paneNamed: #one) port: #selection) value: 3.
	((browser paneNamed: #two) port: #selection) value: 6.
	((browser paneNamed: #three) port: #selection) value: 12.
	((browser paneNamed: #three) port: #focus) value: 12.
	self assert: oneCount equals: 1.
	self assert: outerCount equals: 1