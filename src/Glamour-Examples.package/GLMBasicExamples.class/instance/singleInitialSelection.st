explicit selection
singleInitialSelection
	"
		| browser |
		browser := self new singleInitialSelection.
		browser openOn: 5.
		(browser panes first port: #selection) value: 3.
	"
	|browser |

	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a tree
			display: [:x | 1 to: x];
			format: [:number | Text string: number printString attribute: TextColor red]].
	browser transmit to: #two; from: #one; andShow: [ :a |
		a list
			display: [:x | 1 to: x * 2]].
	^ browser