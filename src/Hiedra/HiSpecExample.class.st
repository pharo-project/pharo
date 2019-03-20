"
I am an example of use of Hiedra in the Spec library (with Morph as backend).
"
Class {
	#name : #HiSpecExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'tree',
		'hiedraColumnController'
	],
	#category : #'Hiedra-UI-Examples'
}

{ #category : #specs }
HiSpecExample class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :col | col add: #tree ];
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
		title: ('Hiedra in Spec - {1} - {2} elements' format: {selector. commits size});
		extent: 500 @ 700;
		openWithSpec
]

{ #category : #accessing }
HiSpecExample >> commits: commits [ 

	tree roots: commits.

	hiedraColumnController ruler: (HiRulerBuilder 
		newRulerValues: commits
		linksBlock: [:aCommit |
			aCommit parentIds collect: [:id | 
				commits detect: [:each | each id = id ] ] ]).

	hiedraColumnController reset.
	
	"After #reset, we can know the width of the column."
	tree columns first initialWidth: hiedraColumnController rulerWidth.
]

{ #category : #initialization }
HiSpecExample >> initializeWidgets [
	
	| rowHeight |
	tree := self newTree.

	"Important: Ensure the height of rows for Hiedra is the same as the widget's cell height."
	rowHeight := 30.

	"Important: Set zero inset, else there will be an empty gap cutting Hiedra ruler."
	tree rowInset: 0.
	
	"Configure hiedra controller"
	hiedraColumnController := HiColumnController new.	
	"Increase the default value since Spec wants all Morphs initially."
	hiedraColumnController pageSize: 1000.
	hiedraColumnController renderer
		rowHeight: rowHeight;
		nodeRadius: 1.5;
		nodeBorderWidth: 3.0;
		arrowSize: 2.5;
		nodeConnectionOffset: 3.5;
		cellWidth: 8;
		nodeColor: Smalltalk ui theme textColor.
	
	"Configure columns"
	tree columns:	{
		TreeColumnPresenter new	
			displayBlock: [ :item |
				(hiedraColumnController cellMorphAtValue: item content)
					height: rowHeight;
					yourself ];
			yourself.
		TreeColumnPresenter new
			displayBlock: [ :item |
				item content comment asStringMorph
					height: rowHeight;
					yourself ];
			yourself.
	}.

]

{ #category : #accessing }
HiSpecExample >> tree [
	^ tree
]
