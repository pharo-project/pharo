actionlist
simpleActionList
	<glmBrowser: 'Action list' input: '#(1 2 3)'>	
	"self new simpleActionList openOn: #(1 2 3)"
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a actionList
			act: [:entity | entity inspect ] entitled: 'Inspect';
			act: [:entity | entity explore ] entitled: 'Explore'].
	^ browser