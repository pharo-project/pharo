others
interdependentPanes
	<glmBrowser: 'Two inter-dependent panes' input: '5'>
	"self new interdependentPanes openOn: 5"
	|browser |
	self flag: 'this example does not work'.
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a | 
		a list
			display: [:x | 1 to: x * 3]].
	browser transmit to: #two; from: #one; andShow: [ :a | 
		a list
			display: [:x | 1 to: x * 3]].
	browser transmit to: #one; from: #two; andShow: [ :a | 
		a list
			display: [:x | 1 to: x * 3]].
	^ browser