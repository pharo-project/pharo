utilities
defaultBrowser 
	| browser |
	browser := GLMTabulator new.
	browser column: #one; column: #two.

	browser transmit to: #one; andShow: [ :a |
		a list
			display: [:x | 1 to: x * 3] ].
	browser transmit to: #two; from: #one; andShow: [ :a |
		a list
			display: [:x | 1 to: x * 3] ].
	browser transmit to: #one; from: #two; andShow: [ :a |
		a list
			display: [:x | 1 to: x * 3] ].

	^ browser