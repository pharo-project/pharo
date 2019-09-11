"
A ChangesBrowser is a browser used to browse a composite change wich gather all the wanted changes
"
Class {
	#name : #ChangesBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'textArea',
		'changesTree',
		'changes'
	],
	#category : #'NautilusRefactoring-Utilities'
}

{ #category : #shortcuts }
ChangesBrowser class >> buildShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #close)
		category: #ChangesBrowserGlobalShortcuts
		default: PharoShortcuts current cancelShortcut
		do: [ :target | target cancel ]
		description: 'Close this dialog'.
		
	(aBuilder shortcut: #accept)
		category: #ChangesBrowserGlobalShortcuts
		default: PharoShortcuts current acceptShortcut
		do: [ :target | target accept ]
		description: 'Accept the proposed changes'.
]

{ #category : #'instance creation' }
ChangesBrowser class >> changes: aCollection [

	^ self new
		changes: aCollection;
		yourself
]

{ #category : #specs }
ChangesBrowser class >> defaultSpec [
	<spec>
	^ SpecLayout composed
		newColumn: [ :column1 | 
			column1
				newColumn: [ :column2 | column2 add: #changesTree ];
				addSplitter;
				add: #textArea ]
]

{ #category : #specs }
ChangesBrowser class >> title [

	^ 'Changes Browser'
]

{ #category : #visiting }
ChangesBrowser >> accept [
	self okToChange
		ifFalse: [ ^ self ].
	[ self pickedChanges do: [ :change | RBRefactoryChangeManager instance performChange: change ] ] asJob
		title: 'Refactoring';
		run.
	self window delete
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

{ #category : #api }
ChangesBrowser >> cancel [
	self window delete
]

{ #category : #accessing }
ChangesBrowser >> changes [
	^ changes
]

{ #category : #accessing }
ChangesBrowser >> changes: aCollection [
	changes := aCollection.
	self updateChanges
]

{ #category : #accessing }
ChangesBrowser >> changesTree [
	^ changesTree
]

{ #category : #initialization }
ChangesBrowser >> initializeChangesTree [
	changesTree
		beCheckList;
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		rootNodeHolder: [ :item | 
					TreeNodePresenter new
						content: item;
						hasContentToShow: true;
						children: [ changesTree childrenFor: item ] ];
		displayBlock: [ :item | item nameToDisplay ].
	self whenBuiltDo: [ changesTree selectAll ]
]

{ #category : #initialization }
ChangesBrowser >> initializePresenter [
	changesTree
		whenHighlightedItemChanged:
			[ :item | item ifNotNil: [ textArea text: (self buildDiffFor: item content) ] ]
]

{ #category : #initialization }
ChangesBrowser >> initializeTextArea [
	textArea 
		enabled: false;
		"make sure we can cycle over the text field"
		bindKeyCombination: Character tab toAction: [ self giveFocusToNextFrom: textArea ];
		bindKeyCombination: Character tab shift toAction: [ self giveFocusToPreviousFrom: textArea ].
]

{ #category : #initialization }
ChangesBrowser >> initializeWidgets [

	changesTree := self newTree.
	textArea := self newCode.
	
	self initializeChangesTree.
	self initializeTextArea.
	self setFocus.
	self
		bindKeyCombination: PharoShortcuts current acceptShortcut toAction: [ self accept ];
		bindKeyCombination: Character escape toAction: [ self cancel ]
]

{ #category : #opening }
ChangesBrowser >> open [
	^ self openDialogWithSpec
		okAction: [ self accept ];
		cancelAction: [ self cancel ];
		yourself
]

{ #category : #accessing }
ChangesBrowser >> pickedChanges [
	| selectedItems |
	selectedItems := changesTree selectedItems collect: #content.
	^ changesTree roots select: [ :i | selectedItems includes: i ]
]

{ #category : #initialization }
ChangesBrowser >> setFocus [
	self focusOrder
		add: changesTree;
		add: textArea;
		yourself
]

{ #category : #initialization }
ChangesBrowser >> setShortcuts [
	self changesTree attachKeymapCategory: #ChangesBrowserGlobalShortcuts targetting: self
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
	changes do: [ :each | aCompositeChange addChange: each ].

	"Later we could filter the shown changes depending on the selected scope"

	changesTree roots: (aCompositeChange whatToDisplayIn: self)
]
