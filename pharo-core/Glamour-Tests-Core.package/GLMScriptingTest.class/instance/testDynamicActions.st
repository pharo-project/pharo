tests
testDynamicActions
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | 
					(a list)
						display: [ :x | 1 to: x ];
						dynamicActions: [ :list | 
									(1 to: list selection)
										collect: [ :index | 
											(GLMGenericAction new)
												action: [ index ];
												title: 'Action' , index printString;
												yourself ] ] ].
	browser startOn: 42.
	(browser panes first port: #selection) value: 3.
	self assert: browser panes first presentations first dynamicActions size equals: 3.
	(browser panes first port: #selection) value: 5.
	self assert: browser panes first presentations first dynamicActions size equals: 5