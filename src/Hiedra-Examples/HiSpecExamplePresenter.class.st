"
I am an example of use of Hiedra in the Spec library (with Morph as backend).
"
Class {
	#name : #HiSpecExamplePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'hiedraColumnController',
		'table',
		'actionBar'
	],
	#category : #'Hiedra-Examples-Spec2'
}

{ #category : #examples }
HiSpecExamplePresenter class >> example60RandomCommits [
	^ self openExample: #randomlyGenerated
]

{ #category : #examples }
HiSpecExamplePresenter class >> openExample: selector [

	| commits |
	commits := HiExampleCommit perform: selector.

	^ self new
		commits: commits;
		open;
		withWindowDo: [ :window |
			window
				extent: 600 @ 800;
				title: ('Hiedra in Spec2 with Roassal3 - {1} - {2} elements' format: {selector. commits size}) ];
		yourself
]

{ #category : #windows }
HiSpecExamplePresenter >> close [

	self window close
]

{ #category : #accessing }
HiSpecExamplePresenter >> commits: commits [

	table items: commits.

	hiedraColumnController ruler: (HiRulerBuilder
		newRulerValues: commits
		linksBlock: [:aCommit |
			aCommit parentIds collect: [:id |
				commits detect: [:each | each id = id ] ] ]).

	hiedraColumnController reset.

	"After #reset, we know the desired width for the column."
	table columns first width: hiedraColumnController rulerWidth
]

{ #category : #layout }
HiSpecExamplePresenter >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: #table;
		add: #actionBar expand: false;
		yourself
]

{ #category : #initialization }
HiSpecExamplePresenter >> forceRefreshHiedra [
	hiedraColumnController reset.
	table withAdapterDo: #refreshList
]

{ #category : #initialization }
HiSpecExamplePresenter >> initializeActionBar [

	| refreshButton inspectButton |
	refreshButton := self newButton
		label: 'Redraw';
		help: 'Refresh the table (and redraw all Hiedra cells with current rendering parameters).';
		action: [ self defer: [ self forceRefreshHiedra ] ];
		yourself.
	inspectButton := self newButton
		label: 'Renderer';
		help: ('Inspect the Hiedra renderer, that has the rendering parameters. Press on {1} button to see the effect of any change on the renderer.' format: { refreshButton label });
		action: [ self defer: [ hiedraColumnController renderer inspect ] ];
		yourself.

	actionBar := self newActionBar
		add: inspectButton;
		add: refreshButton;
		yourself
]

{ #category : #initialization }
HiSpecExamplePresenter >> initializePresenters [
	self initializeTable.
	self initializeActionBar
]

{ #category : #initialization }
HiSpecExamplePresenter >> initializeTable [
	table := self newTable.

	"Configure hiedra controller"
	hiedraColumnController := HiColumnController new.

	hiedraColumnController renderer
		linkWidth: 3.5;
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
		addColumn: (SpImageTableColumn evaluated: [ :item |
			hiedraColumnController cellMorphAtValue: item ]);
		addColumn: (SpStringTableColumn evaluated: [ :item |
			item comment ])
]
