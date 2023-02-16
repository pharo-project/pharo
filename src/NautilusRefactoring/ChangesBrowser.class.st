"
A ChangesBrowser is a browser used to browse a composite change wich gather all the wanted changes
"
Class {
	#name : #ChangesBrowser,
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'changesTree',
		'selectedItems',
		'diffView'
	],
	#category : #'NautilusRefactoring-Utilities'
}

{ #category : #'instance creation' }
ChangesBrowser class >> changes: aCollection [

	^ self on: aCollection
]

{ #category : #visiting }
ChangesBrowser >> accept [

	self okToChange ifFalse: [ ^ self ].

	[ self pickedChanges do: [ :change |
			RBRefactoryChangeManager instance performChange: change ] ] asJob
		title: 'Refactoring';
		run
]

{ #category : #private }
ChangesBrowser >> buildDiffFor: aChange [

	^ aChange ifNil: [ '' ] ifNotNil: [
		  diffView
			  contextClass: aChange changeClass;
			  leftText: aChange oldVersionTextToDisplay;
			  rightText: aChange textToDisplay ]
]

{ #category : #accessing }
ChangesBrowser >> changes [

	^ self model
]

{ #category : #accessing }
ChangesBrowser >> changes: aCollection [

	self model: aCollection
]

{ #category : #accessing }
ChangesBrowser >> changesTree [
	^ changesTree
]

{ #category : #initialization }
ChangesBrowser >> connectPresenters [

	changesTree whenActivatedDo: [ :selection | self buildDiffFor: selection selectedItem ]
]

{ #category : #layout }
ChangesBrowser >> defaultLayout [

	^ SpPanedLayout newTopToBottom
		  add: changesTree;
		  add: diffView;
		  yourself
]

{ #category : #initialization }
ChangesBrowser >> initializeDialogWindow: aWindow [

	super initializeDialogWindow: aWindow.

	aWindow
		title: 'Changes Browser';
		initialExtent: 600 @ 500;
		okAction: [ self accept ]
]

{ #category : #initialization }
ChangesBrowser >> initializePresenters [

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

{ #category : #showing }
ChangesBrowser >> open [

	self openDialog
]

{ #category : #accessing }
ChangesBrowser >> pickedChanges [

	^ changesTree roots select: [ :i | selectedItems includes: i ]
]

{ #category : #accessing }
ChangesBrowser >> textArea [
	^ diffView
]

{ #category : #private }
ChangesBrowser >> updateChanges [
	| aCompositeChange |
	aCompositeChange := RBRefactoryChangeManager changeFactory
		compositeRefactoryChange.
	self changes do: [ :each | aCompositeChange addChange: each ].

	"Later we could filter the shown changes depending on the selected scope"

	changesTree roots: (aCompositeChange whatToDisplayIn: self).
	selectedItems := Set withAll: changesTree roots
]

{ #category : #updating }
ChangesBrowser >> updatePresenter [

	self model ifNil: [ ^ self ].
	self updateChanges
]
