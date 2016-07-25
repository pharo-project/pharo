table
simpleTableWithSorting
	<glmBrowser: 'Table with column sorting' input: '1 to: 20'>
	"
	self new simpleTableWithSorting openOn: (1 to: 20)
	"
	|browser|
	browser := GLMTabulator new.
	browser row: #row.

	browser transmit to: #row; andShow: [ :composite |
		composite table
			column: 'Base 10 (numeric descending sort)' 
				evaluated: [ :each | each asString ] 
				tags: [:item | item even ifTrue: ['even'] ifFalse: ['odd'] ]
				sortedBy: [ :x :y | x > y ] ;
			column: 'Base 16 (default alfabetic ascending sort)' 
				evaluated: [ :each | each printStringBase: 16 ]
				tags: [:item | item < 7 ifTrue: ['<7'] ifFalse: ['>=7' ] ] ].

	^ browser