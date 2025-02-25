"
A ChangesBrowser is a browser used to browse a composite change wich gather all the wanted changes
"
Class {
	#name : 'StChangesBrowserPresenter',
	#superclass : 'SpPresenterWithModel',
	#instVars : [
		'changesTree',
		'selectedItems',
		'diffView'
	],
	#category : 'NautilusRefactoring-Utilities',
	#package : 'NautilusRefactoring',
	#tag : 'Utilities'
}

{ #category : 'instance creation' }
StChangesBrowserPresenter class >> changes: aCollection [

	^ self on: aCollection
]

{ #category : 'visiting' }
StChangesBrowserPresenter >> accept [

	self okToChange ifFalse: [ ^ self ].

	[ RBRefactoryChangeManager instance performChanges: self pickedChanges ] asJob
		title: 'Refactoring';
		run
]

{ #category : 'private' }
StChangesBrowserPresenter >> buildDiffFor: aChange [

	^ aChange ifNil: [ '' ] ifNotNil: [
		  diffView
			  contextClass: aChange changeClass;
			  leftText: aChange oldVersionTextToDisplay;
			  rightText: aChange textToDisplay ]
]

{ #category : 'accessing' }
StChangesBrowserPresenter >> changes [

	^ self model
]

{ #category : 'accessing' }
StChangesBrowserPresenter >> changes: aCollection [

	self model: aCollection
]

{ #category : 'accessing' }
StChangesBrowserPresenter >> changesTree [
	^ changesTree
]

{ #category : 'initialization' }
StChangesBrowserPresenter >> connectPresenters [

	changesTree whenActivatedDo: [ :selection | self buildDiffFor: selection selectedItem ]
]

{ #category : 'layout' }
StChangesBrowserPresenter >> defaultLayout [

	^ SpPanedLayout newTopToBottom
		  add: changesTree;
		  add: diffView;
		  yourself
]

{ #category : 'initialization' }
StChangesBrowserPresenter >> initializeDialogWindow: aWindow [

	super initializeDialogWindow: aWindow.

	aWindow
		title: 'Changes Browser';
		initialExtent: 600 @ 500;
		okAction: [ self accept ]
]

{ #category : 'initialization' }
StChangesBrowserPresenter >> initializePresenters [

	changesTree := self newTreeTable.
	diffView := self newDiff
		            enabled: false;
		            showOptions: true;
		            yourself.

	changesTree
		children: [ :node | (node whatToDisplayIn: self) copyWithout: node ];
		addColumn: (SpCompositeTableColumn
				 with: ((SpCheckBoxTableColumn evaluated: [ :item | selectedItems includes: item ])
						  onActivation: [ :item | selectedItems add: item ];
						  onDeactivation: [ :item | selectedItems remove: item ifAbsent: [  ] ] yourself)
				 with: (SpStringTableColumn evaluated: [ :item | item nameToDisplay ]));
		activateOnSingleClick.

	self focusOrder
		add: changesTree;
		add: diffView
]

{ #category : 'showing' }
StChangesBrowserPresenter >> open [

	self openDialog
]

{ #category : 'accessing' }
StChangesBrowserPresenter >> pickedChanges [

	^ changesTree roots select: [ :i | selectedItems includes: i ]
]

{ #category : 'accessing' }
StChangesBrowserPresenter >> textArea [
	^ diffView
]

{ #category : 'private' }
StChangesBrowserPresenter >> updateChanges [
	| aCompositeChange |
	aCompositeChange := RBRefactoryChangeManager changeFactory
		compositeRefactoryChange.
	self changes do: [ :each | aCompositeChange addChange: each ].

	"Later we could filter the shown changes depending on the selected scope"

	changesTree roots: (aCompositeChange whatToDisplayIn: self).
	selectedItems := Set withAll: changesTree roots
]

{ #category : 'updating' }
StChangesBrowserPresenter >> updatePresenter [

	self model ifNil: [ ^ self ].
	self updateChanges
]
