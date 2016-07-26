browsers
accumulator
	"self new accumulator openOn: 42"

	<glmBrowser: 'Accumulator' input: '42'>
	| browser acc |
	browser := GLMTabulator new.
	acc := GLMAccumulator new.
	acc
		show: [ :a | 
			a title: [ :x | x asString ].
			a list
				display: [ :x | 1 to: x ] ].
	browser
		column: [ :c | 
					c
						row: #one span: 4;
						row: #two ];
		column: #three.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: [ :x | x to: 10 * x ] ].
	(browser transmit)
		from: #one;
		to: #two;
		andShow: [ :a | a text display: [ :x | 'You have selected ' , x printString , '. Double cick to open to the right' ] ].
	"When double-clicking on a number in the list, opens a new tab on the right side"
	(browser transmit)
		from: #one port: #strongSelection;
		to: #three;
		andShowIfNone: [ :a | a custom: acc ].
	"When a number is selected in the list and a corresponding tab already exists on the right side, select the tab"
	(browser transmit)
		from: #one;
		to: #three port: #entityToSelect.
	"When a tab is selected on the right side, select the equivalent line in the first pane"
	(browser transmit)
		from: #three port: #activeEntity;
		to: #one port: #selection.
	^ browser