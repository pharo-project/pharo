*Glamour-FastTable
fastListDropDown
	"self new fastListDropDown openOn: 5"
	<glmBrowser: 'Fast List Drop-down' input: '5'>

	|browser |

	browser := GLMTabulator new.
	browser row: #one size: 30; row: #two.
	browser transmit to: #one; andShow: [:a |
		a dropDownList
			display: [:x | 1 to: x ];
			format: [:number | Text string: number printString attribute: TextColor red]].
	browser transmit from: #one; to: #two; andShow: [:a |
		a fastList
			display: [:x | 1 to: x * 2]].
	^ browser