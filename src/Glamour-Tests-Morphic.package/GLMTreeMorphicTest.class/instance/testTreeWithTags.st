tests
testTreeWithTags
	"Creates a tree and tag some nodes. Then click on a tag and see if the unmatched nodes disappear"

	| browser model oddNode lineWithNumber3 someNode secondSome |
	model := Dictionary new.
	model at: #even put: #(2 6 8).
	model at: #odd put: #(3 7 9).
	model at: #some put: #(1 2 3 4).
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a tree)
						display: [ model keys ];
						sortBlock: [ :x :y | x <= y ];
						children: [ :key | 
									key isNumber
										ifFalse: [ model at: key ] ];
						tags: [ :item | 
									item isNumber
										ifTrue: [ 
											item even
												ifTrue: [ #('even') ]
												ifFalse: [ #('odd') ] ]
										ifFalse: [ #() ] ] ].
	window := browser openOn: model.
	World doOneCycle.
	oddNode := self treeVisibleItems second.	
	
	oddNode toggleExpandedState. "Simulate a click on the node's arrow to expand the node and make sub nodes visible"

	World doOneCycle.
	lineWithNumber3 := self treeVisibleItems third.
	(self find: SimpleButtonMorph in: lineWithNumber3) doButtonAction. "Simulate a click on the 'odd' tag to make even numbers disappear"
	World doOneCycle.
	someNode := self treeVisibleItems fifth.
	someNode toggleExpandedState.
	World doOneCycle.
	secondSome := self find: TextMorph in: self treeVisibleItems seventh.
	self assert: secondSome text asString equals: '3'