others
treeWithAmountFiltering
	<glmBrowser: 'Smart lists' input: '(1 to: 10000)'>
	"self new treeWithAmountFiltering openOn: (1 to: 10000)"
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | 
		a tree
			tags: [ :each | {each even printString} ];
			showOnly: 50 ].
	^ browser 