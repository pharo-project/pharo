*Glamour-FastTable
fastTreeWithLazyChildren
	"self new fastTreeWithLazyChildren openOn: (1 to: 10)"

	<glmBrowser: 'Fast Tree Lazy Children' input: '(1 to: 10)'>
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a fastTree
				title: 'Fast Tree';
				hasChildren: [ :x | x > 2 ];
				children: [ :x | 1 to: x - 1 ] ].
	^ browser