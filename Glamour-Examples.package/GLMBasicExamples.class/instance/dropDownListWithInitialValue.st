explicit selection
dropDownListWithInitialValue
	"self new dropDownListWithInitialValue openOn: 5"

	<glmBrowser: 'Drop-down with initial value' input: '5'>
	|browser |

	browser := GLMTabulator new.
	browser row: #one  size: 30; row: #two.
	browser transmit to: #one; andShow: [:a |
		a dropDownList
			display: [:x | (1 to: x) + 10 ];
			selectedIndex: 2 ].
	browser transmit from: #one; to: #two; andShow: [:a |
		a list
			display: [:x | 1 to: x ]].
	^ browser