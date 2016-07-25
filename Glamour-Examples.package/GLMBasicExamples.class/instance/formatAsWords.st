composite
formatAsWords
	"self new formatAsWords openOn: (1 to: 100)"
	<glmBrowser: 'Format' input: '(1 to: 100)'>	
	| browser |
	browser := GLMTabulator new.
	browser row: #list.
	browser transmit to: #list; andShow: [ :a |
		a tree
			format: [ :x | x asWords];
			display: [:x | x]].
	^ browser 