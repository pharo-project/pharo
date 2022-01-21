"
A ChangesBrowser is a browser used to browse a composite change wich gather all the wanted changes
"
Class {
	#name : #ChangesBrowser,
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'textArea',
		'changesTree',
		'selectedItems'
	],
	#category : #'NautilusRefactoring-Utilities'
}

{ #category : #'instance creation' }
ChangesBrowser class >> changes: aCollection [

	^ self on: aCollection
]

{ #category : #specs }
ChangesBrowser class >> defaultSpec [
	<spec>
	
	^ SpPanedLayout newTopToBottom 
		add: #changesTree;
		add: #textArea;
		yourself
	

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

	^ aChange
		ifNil: [ '' ]
		ifNotNil: [
			TextDiffBuilder 
				buildDisplayPatchFrom: aChange oldVersionTextToDisplay 
				to: aChange textToDisplay ].
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

	changesTree whenSelectionChangedDo: [ :selection |
		selection selectedItem 
			ifNil: [ textArea text: '' ]
			ifNotNil: [ :item | textArea text: (self buildDiffFor: item)  ]
		 ]
]

{ #category : #initialization }
ChangesBrowser >> initializeDialogWindow: aWindow [

	super initializeDialogWindow: aWindow.

	aWindow
		title: 'Changes Browser';
		okAction: [ self accept ]
]

{ #category : #initialization }
ChangesBrowser >> initializePresenters [

	changesTree := self newTreeTable.
	textArea := self newCode.
	textArea enabled: false.

	changesTree
		children: [ :node | 
			(node whatToDisplayIn: self) copyWithout: node ];
		addColumn: (SpCompositeTableColumn
			with: ((SpCheckBoxTableColumn evaluated: [:item | selectedItems includes: item ])
					onActivation: [ :item | selectedItems add: item ];
					onDeactivation: [ :item | selectedItems remove: item ifAbsent: [  ] ]
					yourself)
			with: (SpStringTableColumn evaluated: [ :item | item nameToDisplay ]) ).	

	self focusOrder
		add: changesTree;
		add: textArea
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
	^ textArea
]

{ #category : #private }
ChangesBrowser >> updateChanges [
	| aCompositeChange |
	aCompositeChange := RBRefactoryChangeManager changeFactory
		compositeRefactoryChange.
	self changes do: [ :each | aCompositeChange addChange: each ].

	"Later we could filter the shown changes depending on the selected scope"

	changesTree roots: (aCompositeChange whatToDisplayIn: self).
	selectedItems := Set withAll: changesTree roots.
]

{ #category : #updating }
ChangesBrowser >> updatePresenter [ 

	self model ifNil: [ ^ self ].
	self updateChanges
]
