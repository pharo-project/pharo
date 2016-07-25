explicit selection
multiInitialSelection
	"
		| browser |
		browser := self new multiInitialSelection.
		browser openOn: 5.
		(browser panes first port: #selection) value: #(2 5 3).
	"
	|browser |

	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a tree
			beMultiple;
			display: [:x | 1 to: x * 3]].
	browser transmit to: #two; from: #one; andShow: [ :a |
		a list ].
	^ browser