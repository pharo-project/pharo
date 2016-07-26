explicit selection
treeWithChildrenByLevel
	"
		self new treeWithChildrenByLevel openOn: #(1 2 3 4 5)
	"

	<glmBrowser: 'Tree with children by level' input: '(1 to: 5)'>
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: [ :c | 
					c
						row: #two;
						row: #three ].
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a tree)
						title: 'Tree';
						children: [ :item :x :level | 
									level > 1
										ifTrue: [ #() ]
										ifFalse: [ 1 to: item ] ]	"Children must return a collection" ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | a text title: 'Selection preview' ].
	(browser transmit)
		to: #three;
		from: #one port: #selectionPath;
		andShow: [ :a | a text title: 'Selection path preview' ].
	^ browser