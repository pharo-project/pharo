explicit selection
treeWithTagsMoreLevels
	<glmBrowser: 'Tree withTags more levels' input: '(1 to: 10)'>
	"
		self new treeWithTagsMoreLevels openOn: #(1 2 3 4 5 6 7 8 9 10)
	"
	|browser model|
	model := Dictionary new.
	model at: #some put: #(1 2 3 4).
	model at: #even put: #(2 6 8).
	model at: #odd put: #(3 7 9).
	
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a tree
			display: [model keys];
			children: [:child | child isSymbol
								ifTrue: [Array with: ((model at: child) select: [:c | c < 4]) with: ((model at: child) select: [:c | c >= 4])]
								ifFalse: [child isCollection
											ifTrue: [child]
											ifFalse: [#()]]];
			tags: [:item | Array streamContents: [:stream |
					item isNumber ifTrue: [
						stream nextPut: (item even ifTrue: ['even'] ifFalse: ['odd']).
						stream nextPut: (item < 4 ifTrue: ['<4'] ifFalse: ['>=4' ])]]]].  
	^ browser