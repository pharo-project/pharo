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
	#name : 'SycRefactoringPreviewPresenter2',
	#superclass : 'SpPresenter',
	#instVars : [
		'changes',
		'scopeDropList',
		'table',
		'command',
		'selectedRefactorings',
		'diffPresenter',
		'existsChanges',
		'buttonOk',
		'name'
	],
	#category : 'SystemCommands-RefactoringSupport',
	#package : 'SystemCommands-RefactoringSupport'
}

{ #category : 'layout' }
SycRefactoringPreviewPresenter2 class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: #table;
		add: #scopeDropList withConstraints: [ :constraints | constraints height: 30 ];
		add: #diffPresenter;
		yourself
]

{ #category : 'instance creation' }
SycRefactoringPreviewPresenter2 class >> for: aCommand scopes: scopes [
	^ self new
		command: aCommand;
		scopes: scopes
]

{ #category : 'specs' }
SycRefactoringPreviewPresenter2 class >> title [
	^ 'Refactoring changes'
]

{ #category : 'controlling' }
SycRefactoringPreviewPresenter2 >> accept [
	self okToChange ifFalse: [ ^ self ].
	
	[ ReChangeManager instance
			performChanges: self pickedChanges ] asJob
		title: 'Refactoring';
		run
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> activeRBEnvironment [
	^ self activeScope asRBEnvironment
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> activeScope [
	^ scopeDropList selectedItem
]

{ #category : 'services' }
SycRefactoringPreviewPresenter2 >> alert: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil ifFalse: [ self inform: aString ]
]

{ #category : 'private' }
SycRefactoringPreviewPresenter2 >> buildDiffFor: aChange [
	^ diffPresenter
		leftText: aChange oldVersionTextToDisplay;
		rightText: aChange textToDisplay
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> changeName: aText [
	name := aText .
	self generateChanges
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> changes [
	^ changes
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> changes: aCollection [
	changes := aCollection.
	self generateChanges
]

{ #category : 'utilities' }
SycRefactoringPreviewPresenter2 >> changesFrom: aCompositeChange forEnvironment: anEnvironment [

	^ (aCompositeChange changesToDisplayIn: self) select: [ :change |
		  anEnvironment includesClass: change changeClass ]
]

{ #category : 'initialization' }
SycRefactoringPreviewPresenter2 >> columns [

	^ { (SpCompositeTableColumn new
		   addColumn:
			   ((SpCheckBoxTableColumn evaluated: [ :x | selectedRefactorings at: x ])
				    onActivation: [ :class | self toggleSelectionOf: class ];
				    onDeactivation: [ :class | self toggleSelectionOf: class ];
				    width: 20);
		   addColumn: (SpStringTableColumn evaluated: #name);
		   yourself) }
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> command: aCommand [
	command := aCommand
]

{ #category : 'initialization' }
SycRefactoringPreviewPresenter2 >> connectPresenters [

	table addShortcutWith: [ :action |
			action
				shortcutKey: Character backspace asKeyCombination;
				action: [ self toggleSelectionOf: table selectedItem ] ].

	{
		table.
		scopeDropList.
		diffPresenter } do: [ :each |
			each
				addShortcutWith: [ :action |
						action
							shortcutKey: Character cr asKeyCombination;
							action: [
									self owner
										triggerOkAction;
										close ] ];
				addShortcutWith: [ :action |
						action
							shortcutKey: Character escape asKeyCombination;
							action: [
									self owner
										triggerCancelAction;
										close ] ] ].

	buttonOk action: [ self accept ]
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> existChanges [
	^ existsChanges ifNil: [ existsChanges := false ]
]

{ #category : 'actions' }
SycRefactoringPreviewPresenter2 >> generateChanges [

	| rbEnvironment |
	changes := command asRefactorings.
	rbEnvironment := self activeRBEnvironment.
	changes do: [ :each |
		each model environment: rbEnvironment.
		each generateChanges ]
]

{ #category : 'initialization' }
SycRefactoringPreviewPresenter2 >> initializeDialogWindow: aDialogWindowPresenter [

	aDialogWindowPresenter
		title: self title;
		initialExtent: 500 @ 500;
		addButton: 'Cancel' do: [ :presenter | presenter close ];
		addDefaultButton: 'Apply' do: [ :presenter |
			self accept.
			presenter close ]
]

{ #category : 'initialization - deprecated' }
SycRefactoringPreviewPresenter2 >> initializePresenter [

	super initializePresenter.
	table whenSelectionChangedDo: [ :selection | selection selectedItem ifNotNil: [ self buildDiffFor: selection selectedItem ] ].

	table whenActivatedDo: [ :selectedElement | self toggleSelectionOf: selectedElement widget selection selectedItem ].

	scopeDropList whenSelectedItemChangedDo: [ :scope | self updateChanges ].

	buttonOk := self newButton
		            label: 'Ok';
		            icon: (self iconNamed: #smallOk)
]

{ #category : 'initialization' }
SycRefactoringPreviewPresenter2 >> initializeWidgets [

	diffPresenter := self newDiff.
	scopeDropList := self newDropList.
	table := self newTable.

	table columns: self columns;
		hideColumnHeaders.

	diffPresenter disable.
	scopeDropList display: [ :scope | scope description capitalized ].

	self
		selectectAllCheckBox;
		setFocus
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> name: aText [
	name := aText .
	self generateChanges
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> pickedChanges [
	^ table items select: [ :item | selectedRefactorings at: item ]
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> scopeDropList [
	^ scopeDropList
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> scopes: aCollectionOfScopes [
	scopeDropList items: aCollectionOfScopes "It also sets up first item as selection"
]

{ #category : 'private' }
SycRefactoringPreviewPresenter2 >> selectItemBelow: aRefactoring [

	| nextIndex |
	nextIndex := (self table items indexOf: aRefactoring) + 1.
	self table selectIndex: nextIndex scrollToSelection: true
]

{ #category : 'actions' }
SycRefactoringPreviewPresenter2 >> selectectAllCheckBox [
	selectedRefactorings := Dictionary new.
	table items do: [ :refactoring | selectedRefactorings at: refactoring put: true ]
]

{ #category : 'initialization' }
SycRefactoringPreviewPresenter2 >> setFocus [
	self focusOrder
		add: table;
		add: scopeDropList;
		add: diffPresenter;
		add: name
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> table [
	^ table
]

{ #category : 'accessing' }
SycRefactoringPreviewPresenter2 >> title [
	^ self class title
]

{ #category : 'private' }
SycRefactoringPreviewPresenter2 >> toggleSelectionOf: aRefactoring [
	"it's normal it's impossible that anItem doesn't store in dictionary because at initialize I fill the dictionary and at each scope change"
	selectedRefactorings at: aRefactoring put: (selectedRefactorings at: aRefactoring) not.
	self selectItemBelow: aRefactoring
]

{ #category : 'update' }
SycRefactoringPreviewPresenter2 >> updateChanges [
	self generateChanges.
	self updateTablePresenter.
	self selectectAllCheckBox
]

{ #category : 'update' }
SycRefactoringPreviewPresenter2 >> updateTablePresenter [

	| aCompositeChange anEnvironment |
	"We get the environment to apply the changes from any element in the changes instance variable"
	anEnvironment := self activeRBEnvironment.
	aCompositeChange := ReCompositeChange new
		onSystemEnvironment: anEnvironment.
	changes do: [ :each | aCompositeChange addChange: each ].

	table items: (self changesFrom: aCompositeChange forEnvironment: anEnvironment).
	table items ifNotEmpty: [
		table selectIndex: 1.
		existsChanges := true ]
]
