explicit selection
dropDownList
	"self new dropDownList openOn: 5"

	<glmBrowser: 'Drop-down' input: '5'>
	|browser |

	browser := GLMTabulator new.
	browser row: #one size: 30; row: #two.
	browser transmit to: #one; andShow: [:a |
		a dropDownList
			display: [:x | 1 to: x ];
			format: [:number | Text string: number printString attribute: TextColor red]].
	browser transmit from: #one; to: #two; andShow: [:a |
		a list
			display: [:x | 1 to: x * 2]].
	^ browser