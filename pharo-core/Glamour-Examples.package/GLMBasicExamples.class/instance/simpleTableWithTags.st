table
simpleTableWithTags
	<glmBrowser: 'Table with tags' input: '1 to: 20'>
	"
	self new simpleTableWithTags openOn: (1 to: 20)
	"
	|browser|
	browser := GLMTabulator new.
	browser row: #row.

	browser transmit to: #row; andShow: [ :composite |
		composite table
			column: 'Base 8' 
				evaluated: [ :each | each printStringBase: 8 ] 
				tags: [:item | 
					Array streamContents: [:stream |
						item isNumber ifTrue: [
							stream nextPut: (item even ifTrue: ['even'] ifFalse: ['odd']).
							stream nextPut: (item < 3 ifTrue: ['<3'] ifFalse: ['>=3' ]) ] ] ];
			column: 'Base 10' 
				evaluated: [ :each | each asString ] 
				tags: [:item | 
					Array streamContents: [:stream |
						item isNumber ifTrue: [
							stream nextPut: (item even ifTrue: ['even'] ifFalse: ['odd']).
							stream nextPut: (item < 9 ifTrue: ['<9'] ifFalse: ['>=9' ])] ] ];
			column: 'Base 16' 
				evaluated: [ :each | each printStringBase: 16 ]
				tags: [:item | 
					Array streamContents: [:stream |
						item isNumber ifTrue: [
							stream nextPut: (item < 4 ifTrue: ['<4'] ifFalse: ['>=4' ]).
							stream nextPut: (item < 7 ifTrue: ['<7'] ifFalse: ['>=7' ])] ] ] ].
	^ browser