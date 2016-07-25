others
doubleClick
	"self new doubleClick openOn: 100"

	<glmBrowser: 'Double click' input: '100'>
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #one;
		andShow: [ :a | 
			a table display: [ :x | 1 to: x ]; 
				column: 'value' evaluated: #asString; 
				column: 'odd' evaluated: [:each | each odd asString ]].
	browser
		transmit to: #two;
		from: #one port: #strongSelection;
		andShow: [:a |  a list display: [ :x | 1 to: x ] ].
	^ browser