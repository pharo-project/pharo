"
I am a GUI used to preview the impact of a refactoring and select the changes to apply.

Examples

	scopePackage := ClyPackageScope of: 'Stage' asPackage in: ClyNavigationEnvironment currentImage.
	scopeClass := ClyClassScope of: 'SycRefactoringPreview' in: ClyNavigationEnvironment currentImage.
	
	SycRefactoringPreview2 for:  scope: { scopePackage . scopeClass } 



Instance Variables
	changes:						<ArrayOfRBRefactroring>	The collection of changes that can be applied with the refactoring.
	scopeDropList:				<DropListPresenter>		A drop list to let the user chose the scope of the refactoring to apply. (All image, current package, current class...)				
	selectedRefactorings:		<Dictionary>					A dictionary remembering for all refactorings if they should be applied.	
	table:							<TablePresenter>			A table to display the possible refactorings.
	diffPresenter				<DiffPresenter>				A diff presenter to show the users the consequences of the refactorings.
	
"
Class {
	#name : #SycRefactoringPreview,
	#superclass : #SpPresenter,
	#instVars : [
		'changes',
		'scopeDropList',
		'table',
		'command',
		'selectedRefactorings',
		'diffPresenter',
		'existsChanges'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
SycRefactoringPreview class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #table;
		add: #scopeDropList withConstraints: [ :constraints | constraints height: 30 ];
		add: #diffPresenter;
		yourself
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
	self okToChange ifFalse: [ ^ self ].
	[ self pickedChanges do: [ :change | RBRefactoryChangeManager instance performChange: change ] ] asJob
		title: 'Refactoring';
		run
]

{ #category : #accessing }
SycRefactoringPreview >> activeRBEnvironment [
	^ self activeScope asRBEnvironment
]

{ #category : #accessing }
SycRefactoringPreview >> activeScope [
	^ scopeDropList selectedItem
]

{ #category : #services }
SycRefactoringPreview >> alert: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil
		ifFalse: [ UIManager default alert: aString ]
]

{ #category : #'epicea support' }
SycRefactoringPreview >> asEpiceaEvent [
	"thanks to this method epicia know that is a refactoring"
	
	self flag: #todo. "This is a workaround for a problem found by RewriteTool"
	(changes allSatisfy: [ :each | each respondsTo: #asEpiceaEvent ])
		ifFalse: [ ^ EpCompositeRefactoring withAll: #() ].

	^ changes size > 1
		ifTrue: [ EpCompositeRefactoring withAll: changes ]
		ifFalse: [ changes anyOne asEpiceaEvent ].
]

{ #category : #private }
SycRefactoringPreview >> buildDiffFor: aChange [
	^ diffPresenter
		leftText: aChange oldVersionTextToDisplay;
		rightText: aChange textToDisplay
]

{ #category : #accessing }
SycRefactoringPreview >> changes [
	^ changes
]

{ #category : #accessing }
SycRefactoringPreview >> changes: aCollection [
	changes := aCollection.
	self generateChanges 
]

{ #category : #accessing }
SycRefactoringPreview >> command: aCommand [
	command := aCommand
]

{ #category : #accessing }
SycRefactoringPreview >> existChanges [
	^ existsChanges ifNil: [ existsChanges := false ]
]

{ #category : #update }
SycRefactoringPreview >> generateChanges [

	| rbEnvironment |
	changes := command asRefactorings.
	rbEnvironment := self activeRBEnvironment.
	changes do: [ :each | 
		each model environment: rbEnvironment.
		each primitiveExecute ]
]

{ #category : #initialization }
SycRefactoringPreview >> initializeDialogWindow: aDialogWindowPresenter [
	aDialogWindowPresenter
		title: self title;
		initialExtent: 500 @ 500;
		addButton: 'Apply'
			do: [ :presenter | 
			self accept.
			presenter close ];
		addButton: 'Cancel' do: [ :presenter | presenter close ]
]

{ #category : #initialization }
SycRefactoringPreview >> initializePresenter [
	super initializePresenter.
	table
		whenSelectionChangedDo: [ :selection | self buildDiffFor: selection selectedItem ].
	table
		whenActivatedDo: [ :selectedElement | 
			self toggleSelectionOf: selectedElement widget selection selectedItem.
			self rebuild ].
	scopeDropList
		whenSelectedItemChangedDo: [ :scope | self updateChanges ]
]

{ #category : #initialization }
SycRefactoringPreview >> initializeWidgets [
	table := self newTable.
	diffPresenter := self newDiff.
	scopeDropList := self newDropList.

	table
		addColumn:
			((SpCheckBoxTableColumn evaluated: [ :x | selectedRefactorings at: x ])
				onActivation: [ :class | self toggleSelectionOf: class ];
				onDesactivation: [ :class | self toggleSelectionOf: class ];
				width: 20);
		addColumn: (SpStringTableColumn evaluated: #name);
		hideColumnHeaders.

	diffPresenter disable.
	scopeDropList display: [ :scope | scope description capitalized ].

	self
		selectectAllCheckBox;
		setShortcuts;
		setFocus
]

{ #category : #'epicea support' }
SycRefactoringPreview >> isEpiceaInterestingJobOwner [

	^ true
]

{ #category : #accessing }
SycRefactoringPreview >> pickedChanges [
	^ table items select: [ :item | selectedRefactorings at: item ]
]

{ #category : #update }
SycRefactoringPreview >> rebuild [
	self needRebuild: false.
	self buildWithSpec
]

{ #category : #accessing }
SycRefactoringPreview >> scopeDropList [
	^ scopeDropList
]

{ #category : #accessing }
SycRefactoringPreview >> scopes: aCollectionOfScopes [
	scopeDropList items: aCollectionOfScopes "It also sets up first item as selection"
]

{ #category : #update }
SycRefactoringPreview >> selectectAllCheckBox [
	selectedRefactorings := Dictionary new.
	table items do: [ :refactoring | selectedRefactorings at: refactoring put: true ]
]

{ #category : #initialization }
SycRefactoringPreview >> setFocus [
	self focusOrder
		add: table;
		add: scopeDropList;
		add: diffPresenter
]

{ #category : #initialization }
SycRefactoringPreview >> setShortcuts [
	self
		bindKeyCombination: PharoShortcuts current acceptShortcut toAction: [ self accept ];
		bindKeyCombination: Character escape toAction: [ self close ]
]

{ #category : #accessing }
SycRefactoringPreview >> table [
	^ table
]

{ #category : #accessing }
SycRefactoringPreview >> title [
	^ self class title
]

{ #category : #private }
SycRefactoringPreview >> toggleSelectionOf: aRefactoring [
	"it's normal it's impossible that anItem dosn't store in dictionary because at initialize i fill the dictionary and at each scope change"

	selectedRefactorings at: aRefactoring put: (selectedRefactorings at: aRefactoring) not
]

{ #category : #update }
SycRefactoringPreview >> updateChanges [
	self generateChanges.
	self updateTablePresenter.
	self selectectAllCheckBox
]

{ #category : #update }
SycRefactoringPreview >> updateTablePresenter [
	| aCompositeChange |
	aCompositeChange := RBCompositeRefactoryChange new.
	changes do: [ :each | aCompositeChange addChange: each ].
	table
		items: (aCompositeChange whatToDisplayIn: self).
	table items ifNotEmpty: [ table selectIndex: 1.
		existsChanges := true ]
]
