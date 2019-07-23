"
I am a GUI used to preview the impact of a refactoring and select the changes to apply.

"
Class {
	#name : #ChangesBrowserPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'textArea',
		'changesTree',
		'changes',
		'selectedChanges'
	],
	#category : #'Refactoring-Missing'
}

{ #category : #shortcuts }
ChangesBrowserPresenter class >> buildShortcutsOn: aBuilder [
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

{ #category : #shortcuts }
ChangesBrowserPresenter class >> changes: aCollection [

	^ self new
		changes: aCollection;
		yourself
]

{ #category : #shortcuts }
ChangesBrowserPresenter class >> defaultSpec [
	<spec: #default>
	
	^ SpecBoxLayout newVertical add: (SpecBoxLayout newHorizontal
				add: #changesTree; yourself);
				add: #textArea;
				yourself 
]

{ #category : #shortcuts }
ChangesBrowserPresenter class >> title [

	^ 'Changes Browser'
]

{ #category : #visiting }
ChangesBrowserPresenter >> accept [
	self okToChange
		ifFalse: [ ^ self ].
	[ self pickedChanges do: 
		[ :change | RBRefactoryChangeManager instance performChange: change ] 
	] asJob
		title: 'Refactoring';
		run.
	self window delete
]

{ #category : #visiting }
ChangesBrowserPresenter >> asEpiceaEvent [
	self flag: #pharoTodo.	"This is a workaround for a problem found by RewriteTool"
	(changes allSatisfy: [ :each | each respondsTo: #asEpiceaEvent ]) ifFalse: [ ^ EpCompositeRefactoring withAll: #() ].

	^ changes size > 1 ifTrue: [ EpCompositeRefactoring withAll: changes ] ifFalse: [ changes anyOne asEpiceaEvent ]
]

{ #category : #visiting }
ChangesBrowserPresenter >> buildDiffFor: aChange [
	textArea 
		leftText: aChange oldVersionTextToDisplay;
		rightText: aChange textToDisplay
]

{ #category : #visiting }
ChangesBrowserPresenter >> cancel [
	self window delete
]

{ #category : #visiting }
ChangesBrowserPresenter >> changes [
	^ changes
]

{ #category : #visiting }
ChangesBrowserPresenter >> changes: aCollection [
	changes := aCollection.
	self updateChanges
]

{ #category : #visiting }
ChangesBrowserPresenter >> changesTree [
	^ changesTree
]

{ #category : #visiting }
ChangesBrowserPresenter >> initializeChangesTree [
	changesTree addColumn:
		((CheckBoxTableColumn title: ' ' evaluated: [ :class | selectedChanges includes: class ])
				onActivation: [ :class | selectedChanges add: class.
					self buildDiffFor: class ];
				onDesactivation: [ :class | selectedChanges remove: class ];
				width: 20;
				yourself);
			addColumn:  (StringTableColumn title: 'Name' evaluated: #name);
			beResizable.
	self whenBuiltDo: [ selectedChanges := changesTree items ]
]

{ #category : #visiting }
ChangesBrowserPresenter >> initializePresenter [
	changesTree
		whenSelectionChangedDo:
			[ :item | item ifNotNil: [ self buildDiffFor: 
				(item widget items at: (item selectedIndex))] ]
]

{ #category : #visiting }
ChangesBrowserPresenter >> initializeTextArea [
	textArea 
		enabled: false;
		"make sure we can cycle over the text field"
		bindKeyCombination: Character tab toAction: [ self giveFocusToNextFrom: textArea ];
		bindKeyCombination: Character tab shift toAction: [ self giveFocusToPreviousFrom: textArea ].
]

{ #category : #visiting }
ChangesBrowserPresenter >> initializeWidgets [
	selectedChanges := OrderedCollection new.
	changesTree := self newTable.
	textArea := self newDiff.
	
	self initializeChangesTree.
	self setFocus.
	self
		bindKeyCombination: PharoShortcuts current acceptShortcut toAction: [ self accept ];
		bindKeyCombination: Character escape toAction: [ self cancel ]
]

{ #category : #visiting }
ChangesBrowserPresenter >> isEpiceaInterestingJobOwner [

	^ true
]

{ #category : #visiting }
ChangesBrowserPresenter >> open [
	^ self openDialogWithSpec
		okAction: [ self accept ];
		cancelAction: [ self cancel ];
		initialExtent: 600 @ 300;
		yourself
]

{ #category : #visiting }
ChangesBrowserPresenter >> pickedChanges [
	^ changesTree items select: [ :i | selectedChanges includes: i ]
]

{ #category : #visiting }
ChangesBrowserPresenter >> setFocus [
	self focusOrder
		add: changesTree;
		add: textArea;
		yourself
]

{ #category : #visiting }
ChangesBrowserPresenter >> setShortcuts [
	self changesTree attachKeymapCategory: #ChangesBrowserGlobalShortcuts targetting: self
]

{ #category : #visiting }
ChangesBrowserPresenter >> textArea [
	^ textArea
]

{ #category : #visiting }
ChangesBrowserPresenter >> updateChanges [
	| aCompositeChange |
	aCompositeChange := RBRefactoryChangeManager changeFactory
		compositeRefactoryChange.
	changes do: [ :each | aCompositeChange addChange: each ].
	"Later we could filter the shown changes depending on the selected scope"
	
	changesTree items: (aCompositeChange whatToDisplayIn: self).
]
