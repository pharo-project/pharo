explicit selection
treeWithTags
	<glmBrowser: 'Tree withTags' input: '(1 to: 10)'>
	"
		self new treeWithTags openOn: #(1 2 3 4 5 6 7 8 9 10)
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
			children: [:key | key isNumber ifFalse: [model at: key] ];
			tags: [:item | item isNumber ifTrue: [item even ifTrue: [#('even')] ifFalse: [#('odd')]] ifFalse: [#()]]].  
	^ browser