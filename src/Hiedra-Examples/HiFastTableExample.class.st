"
I am an example of use of Hiedra in a FastTable.
"
Class {
	#name : #HiFastTableExample,
	#superclass : #FTSimpleDataSource,
	#instVars : [
		'hiedraColumnController'
	],
	#category : #'Hiedra-Examples'
}

{ #category : #examples }
HiFastTableExample class >> example2870IcebergCommits [
	self openExample: #iceberg_v1_5_6
]

{ #category : #examples }
HiFastTableExample class >> example493PharoCommits [
	self openExample: #pharo_v60507
]

{ #category : #examples }
HiFastTableExample class >> example60RandomCommits [
	self openExample: #randomlyGenerated
]

{ #category : #examples }
HiFastTableExample class >> openExample: selector [

	| commits dataSource table |
	commits := HiExampleCommit perform: selector.
	dataSource := self new.

	table := FTTableMorph new
		extent: 500 @ 700;
		addColumn: (FTColumn id: ' ');
		addColumn: (FTColumn id: 'Comment');
		dataSource: dataSource;
		beResizable;
		yourself.
	
	dataSource elements: commits.
	
	table openInWindowLabeled: ('Hiedra in FastTable - {1} - {2} elements' format: {selector. commits size})
]

{ #category : #accessing }
HiFastTableExample >> cellColumn: column row: rowIndex [
	"Answer a morph with the cell view."
	
	column id = ' ' ifTrue: [ 
		^ FTCellMorph new
			addMorphBack: (hiedraColumnController cellMorphAtRow: rowIndex);
			yourself
		].

	column id = 'Comment' ifTrue: [ 
		| commit |
		commit := self elementAt: rowIndex.
		^ commit comment asMorph ].

	^ self error
]

{ #category : #accessing }
HiFastTableExample >> elements: commits [
	super elements: commits.

	"Adjust the ruler rendering settings with table's row height."
	hiedraColumnController renderer rowHeight: self table rowHeight floor.

	"Create a new ruler model with the new elements"
	hiedraColumnController ruler: (HiRulerBuilder 
		newRulerValues: commits
		linksBlock: [:aCommit |
			aCommit parentIds collect: [:id | 
				commits detect: [:each | each id = id ] ] ]).

	"Reset the visualization"
	hiedraColumnController reset.

	"Adjust the column width in the table"
	self table columns first width: hiedraColumnController rulerWidth.

]

{ #category : #initialization }
HiFastTableExample >> initialize [
	super initialize.
	hiedraColumnController := HiColumnController new.
	hiedraColumnController renderer
		arrowSize: 2;
		nodeConnectionOffset: 2;
		cellWidth: 7.

]
