"
I am an example of use of Hiedra in the Spec library (with Morph as backend).
"
Class {
	#name : #HiSpecExample,
	#superclass : #SpPresenter,
	#instVars : [
		'hiedraColumnController',
		'table'
	],
	#category : #'Hiedra-Examples'
}

{ #category : #specs }
HiSpecExample class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #table;
		yourself
]

{ #category : #specs }
HiSpecExample class >> example2870IcebergCommits [
	self openExample: #iceberg_v1_5_6
]

{ #category : #specs }
HiSpecExample class >> example493PharoCommits [
	self openExample: #pharo_v60507
]

{ #category : #specs }
HiSpecExample class >> example60RandomCommits [
	self openExample: #randomlyGenerated
]

{ #category : #specs }
HiSpecExample class >> openExample: selector [

	| commits |
	commits := HiExampleCommit perform: selector.

	self new
		commits: commits;
		openWithSpec;
		withWindowDo: [ :window |
			window
				extent: 600 @ 800;
				title: ('Hiedra in Spec2 with Roassal3 - {1} - {2} elements' format: {selector. commits size}) ].

]

{ #category : #accessing }
HiSpecExample >> commits: commits [ 

	table items: commits.

	hiedraColumnController ruler: (HiRulerBuilder 
		newRulerValues: commits
		linksBlock: [:aCommit |
			aCommit parentIds collect: [:id | 
				commits detect: [:each | each id = id ] ] ]).

	hiedraColumnController reset.
	
	"After #reset, we can know the width of the column."
	table columns first width: hiedraColumnController rulerWidth.
]

{ #category : #initialization }
HiSpecExample >> initializeWidgets [
	table := self newTable.

	"Configure hiedra controller"
	hiedraColumnController := HiColumnController new.

	hiedraColumnController renderer
		nodeRadius: 1.5;
		nodeBorderWidth: 3.0;
		arrowSize: 0;
		nodeConnectionOffset: 3;
		cellWidth: 8;
		useUniformColorStrategy.

	"Adjust the ruler rendering settings with table's row height."
	hiedraColumnController renderer rowHeight: FTTableMorph defaultRowHeight floor.

	"Configure columns"
	table
		addColumn: (SpImageTableColumn evaluated: [ :item | hiedraColumnController cellMorphAtValue: item ]);
		addColumn: (SpStringTableColumn evaluated: #comment)
]
