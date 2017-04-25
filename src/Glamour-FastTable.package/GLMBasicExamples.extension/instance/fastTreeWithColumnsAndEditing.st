*Glamour-FastTable
fastTreeWithColumnsAndEditing
	"self new fastTreeWithColumnsAndEditing openOn: ((1 to: 5) collect: [ :index1 |
	((1 to: 10) collect: [ :index | index -> (ValueHolder new contents: 'description') ]) asDictionary ]) "

	<glmBrowser: 'Fast Tree With Editable Columns' input: '((1 to: 5) collect: [ :index1 |
	((1 to: 10) collect: [ :index | index -> (ValueHolder new contents: ''description'') ]) asDictionary ])'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastTreeTable 
			children: [ :x | x isDictionary ifTrue: [ x associations ] ifFalse: [ #() ] ];
			column: [:x | 'Numbers'] evaluated: [ :value | value isDictionary ifTrue: [ value ] ifFalse: [ value key ] ];
			column: 'Description' 
			   evaluated: [ :value | value isDictionary ifTrue: [ '' ] ifFalse: [ value value contents ] ] 
			   modified: [ :newText :value :presentation | 
					value isDictionary ifFalse: [ value value contents: newText  ]] ].
	^ browser