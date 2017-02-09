*Glamour-FastTable
fastListMultipleSelection
	"self new fastListMultipleSelection openOn: 10"
	<glmBrowser: 'Fast List Multiple Selection' input: '10'>

	|browser |

	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [:a |
		a fastList
			beMultiple;
			display: [:x | 1 to: x ]].
	browser transmit from: #one; to: #two; andShow: [:a |
		a fastList ].
	^ browser