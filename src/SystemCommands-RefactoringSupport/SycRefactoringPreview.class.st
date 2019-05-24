"
I am refactoring preview which is created on the instance of CmdCommand which provide refactorings.
Such commands should implemetn method: 

- asRefactorings
It should return collection of refactorings to complete the command.

I was copied from the ChangesBrowser.
But I add extra widget to the view to allow scope refactorings. 
Scope instances should understand two messages: 

- description to be represented in drop down list.
- asRBEnvironment to restrict refactoring instances using RBBrowserEnvironment logic.

Currently only Calypso provide such scopes. 

Also in contrast to ChangesBrowser I do not allow show arbitrary changes. I should always be created on the command:

	SycRefactoringPreview for: aCommand scopes: scopes.

Internal Representation and Key Implementation Points.

    Instance Variables
	changes:		<Object>
	changesTree:		<Object>
	command:		<CmdCommand>
	scopeDropList:		<Object>
	scopes:		<Collection>
	textArea:		<Object>

    Implementation Points
"
Class {
	#name : #SycRefactoringPreview,
	#superclass : #ComposableModel,
	#instVars : [
		'command',
		'scopes',
		'textArea',
		'changesTree',
		'changes',
		'scopeDropList'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #shortcuts }
SycRefactoringPreview class >> buildShortcutsOn: aBuilder [
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

{ #category : #specs }
SycRefactoringPreview class >> defaultSpec [
	<spec>
	^ SpecLayout composed
		newColumn: [ :column1 | 
			column1
				newRow: [ :column2 | column2 add: #changesTree ];
				newRow: [ :column2 | column2 add: #scopeDropList ] height: 30;
				addSplitter;
				add: #textArea ]
]

{ #category : #'instance creation' }
SycRefactoringPreview class >> for: aCommand scopes: scopes [

	^ self new
		command: aCommand;
		scopes: scopes
]

{ #category : #specs }
SycRefactoringPreview class >> title [

	^ 'Refactoring changes'
]

{ #category : #controlling }
SycRefactoringPreview >> accept [
	self okToChange
		ifFalse: [ ^ self ].
	[ self pickedChanges do: [ :change | RBRefactoryChangeManager instance performChange: change ] ] asJob
		title: 'Refactoring';
		run.
	self window delete
]

{ #category : #private }
SycRefactoringPreview >> activeRBEnvironment [
	
	^self activeScope asRBEnvironment
]

{ #category : #accessing }
SycRefactoringPreview >> activeScope [
	^scopeDropList selectedItem
]

{ #category : #'epicea support' }
SycRefactoringPreview >> asEpiceaEvent [

	self flag: #todo. "This is a workaround for a problem found by RewriteTool"
	(changes allSatisfy: [ :each | each respondsTo: #asEpiceaEvent ])
		ifFalse: [ ^ EpCompositeRefactoring withAll: #() ].

	^ changes size > 1
		ifTrue: [ EpCompositeRefactoring withAll: changes ]
		ifFalse: [ changes anyOne asEpiceaEvent ].
]

{ #category : #private }
SycRefactoringPreview >> buildDiffFor: aChange [

	^ aChange
		ifNil: [ '' ]
		ifNotNil: [
			TextDiffBuilder 
				buildDisplayPatchFrom: aChange oldVersionTextToDisplay 
				to: aChange textToDisplay ].
]

{ #category : #controlling }
SycRefactoringPreview >> cancel [
	self window delete
]

{ #category : #accessing }
SycRefactoringPreview >> changes [
	^ changes
]

{ #category : #accessing }
SycRefactoringPreview >> changes: aCollection [
	changes := aCollection
]

{ #category : #accessing }
SycRefactoringPreview >> changesTree [
	^ changesTree
]

{ #category : #accessing }
SycRefactoringPreview >> command [
	^ command
]

{ #category : #accessing }
SycRefactoringPreview >> command: aCommand [
	command := aCommand
]

{ #category : #private }
SycRefactoringPreview >> generateChanges [

	| rbEnvironment |
	changes := command asRefactorings.
	rbEnvironment := self activeRBEnvironment.
	changes do: [ :each | 
		each model environment: rbEnvironment.
		each primitiveExecute ]
]

{ #category : #initialization }
SycRefactoringPreview >> initializeChangesTree [
	changesTree
		beCheckList;
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		rootNodeHolder: [ :item | 
					TreeNodeModel new
						content: item;
						hasContentToShow: true;
						children: [ changesTree childrenFor: item ] ];
		displayBlock: [ :item | item nameToDisplay ].
	self whenBuiltDo: [ changesTree selectAll ]
]

{ #category : #initialization }
SycRefactoringPreview >> initializePresenter [
	changesTree whenHighlightedItemChanged: [ :item | item ifNotNil: [ textArea text: (self buildDiffFor: item content) ] ]
]

{ #category : #initialization }
SycRefactoringPreview >> initializeScopeDropList [
	scopeDropList
		displayBlock: [ :scope | scope description capitalized ];
		whenSelectedItemChanged: [ :scope | 
			scope ifNotNil: [self updateChanges] ].
]

{ #category : #initialization }
SycRefactoringPreview >> initializeTextArea [
	textArea 
		enabled: false;
		"make sure we can cycle over the text field"
		bindKeyCombination: Character tab toAction: [ self giveFocusToNextFrom: textArea ];
		bindKeyCombination: Character tab shift toAction: [ self giveFocusToPreviousFrom: textArea ].
]

{ #category : #initialization }
SycRefactoringPreview >> initializeWidgets [

	changesTree := self newTree.
	textArea := self newCode.
	scopeDropList := self newDropList.
	
	self initializeChangesTree.
	self initializeTextArea.
	self initializeScopeDropList.
	self setFocus.
	self
		bindKeyCombination: PharoShortcuts current acceptShortcut toAction: [ self accept ];
		bindKeyCombination: Character escape toAction: [ self cancel ]
]

{ #category : #'epicea support' }
SycRefactoringPreview >> isEpiceaInterestingJobOwner [

	^ true
]

{ #category : #controlling }
SycRefactoringPreview >> open [
	^ self openDialogWithSpec
		okAction: [ self accept ];
		cancelAction: [ self cancel ];
		yourself
]

{ #category : #accessing }
SycRefactoringPreview >> pickedChanges [
	| selectedItems |
	selectedItems := changesTree selectedItems collect: #content.
	^ changesTree roots select: [ :i | selectedItems includes: i ]
]

{ #category : #accessing }
SycRefactoringPreview >> scopeDropList [
	^ scopeDropList
]

{ #category : #accessing }
SycRefactoringPreview >> scopes [
	^ scopes
]

{ #category : #accessing }
SycRefactoringPreview >> scopes: anObject [
	scopes := anObject.
	scopeDropList items: scopes "It also sets up first item as selection"
]

{ #category : #initialization }
SycRefactoringPreview >> setFocus [
	self focusOrder
		add: changesTree;
		add: textArea;
		yourself
]

{ #category : #initialization }
SycRefactoringPreview >> setShortcuts [
	self changesTree attachKeymapCategory: #ChangesBrowserGlobalShortcuts targetting: self
]

{ #category : #accessing }
SycRefactoringPreview >> textArea [
	^ textArea
]

{ #category : #private }
SycRefactoringPreview >> updateChanges [
	| aCompositeChange |
	self generateChanges.
	aCompositeChange := RBCompositeRefactoryChange new.
	changes do: [:each | aCompositeChange addChange: each ].
	
	changesTree roots: (aCompositeChange whatToDisplayIn: self).
	changesTree selectAll
]
