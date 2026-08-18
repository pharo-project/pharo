"
I am a tool that previews refactoring changes to the user. User can change the scope of the refactoring (default scopes are: class, hierarchy, package and image) and manually pick which changes to apply and which ones to skip. I am usually opened by `RBDriver` subclasses.

I am responsible for keeping track of `selectedChanges` (which changes to apply). When user selects a change from the list of all changes, I display a diff between old and new version of that change. When user accepts changes he wants to execute, I am responsible for invoking `RBRefactoryChangeManager` to perform them.

"
Class {
	#name : 'StRenameClassPreviewPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'changes',
		'changesInScope',
		'selectedChanges',
		'scopes',
		'activeScope',
		'table',
		'diffPresenter',
		'scopeLabel',
		'scopeDropList',
		'buttonOk',
		'buttonCancel',
		'compositeChange',
		'refactoring',
		'name',
		'newNameLabel',
		'newNameInput',
		'driver',
		'validationLabel',
		'previewUpdateDelay',
		'nameIsValid',
		'mainLayout',
		'topLayout'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'instance creation' }
StRenameClassPreviewPresenter class >> withChanges: changes scopes: scopes [
	
	^ self new
		  changesFrom: changes scopes: scopes;
		  yourself
]

{ #category : 'action' }
StRenameClassPreviewPresenter >> accept [
	
	(self okToChange not or: [ selectedChanges isEmptyOrNil ]) ifTrue: [
		self application newInform 
				title: 'No changes to apply.';
				acceptLabel: 'Close this!';
				openDialog.
		
		self closeWindow ].
	self performChanges.
	self updateGlobalActiveScope.
	self closeWindow
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> changesFrom: aCompositeChange [

	compositeChange := aCompositeChange.
	"we keep the composite because we want to use it for performing changes later on."
	
	changes := aCompositeChange changesToDisplayIn: self.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> changesFrom: aCompositeChange scopes: aCollection [

	scopes := aCollection.
	self changesFrom: aCompositeChange.
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> clearDiff [

	diffPresenter 
		leftText: '';
		rightText: ''
]

{ #category : 'action' }
StRenameClassPreviewPresenter >> closeWindow [

	self withWindowDo: [ :window | window close ]
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> connectPresenters [

	table whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [
			self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope | 
			self 
				updateActiveScope; 
				updateChanges; 
				updateTablePresenter ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ] .
	
	newNameInput whenTextChangedDo: [ :text |
		self schedulePreviewUpdate: text ] 
]

{ #category : 'api - focus' }
StRenameClassPreviewPresenter >> defaultKeyboardFocus [

	^ buttonOk
]

{ #category : 'layout' }
StRenameClassPreviewPresenter >> defaultLayout [
	| scopesLayout nameLayout inputLayout |
	
	scopesLayout := SpBoxLayout newLeftToRight
		vAlignCenter;
		spacing: 5;
		add: scopeLabel expand: false;
		add: scopeDropList;
		yourself.
		
	inputLayout := SpBoxLayout newTopToBottom 
		spacing: 2;
		add: newNameInput expand: false;
		add: validationLabel expand: false; 
		yourself. 
		
	nameLayout := SpBoxLayout newLeftToRight 
		spacing: 5;
		add:newNameLabel expand: false; 
		add: inputLayout;
		yourself.
		
	topLayout := SpBoxLayout newTopToBottom 
		spacing: 10;
		add: nameLayout expand: false;
		yourself.
	
	^ SpBoxLayout newTopToBottom
		spacing: 10;
		add: topLayout expand: false;
		add: scopesLayout expand: false;
		add: table;
		add: diffPresenter;
		yourself.

]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> driver: aDriver [
	driver := aDriver
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> initializeDialogWindow: aDialogWindowPresenter [
	"Used to initialize the model in the case of the use into a dialog window.
	 Override this to set buttons other than the default (Ok, Cancel)."
	
	aDialogWindowPresenter
		addButton: 'Cancel' do: [ :presenter | presenter close ];
		addDefaultButton: 'Ok' do: [ :presenter | 
			self accept.
			presenter close ]
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> initializePresenters [

	diffPresenter := self newDiff.

	table := self newTable
		hideColumnHeaders;
		addColumn: (SpCompositeTableColumn new
			addColumn: ((SpCheckBoxTableColumn evaluated: [ :change | selectedChanges includes: change ])
				onActivation: [ :change | selectedChanges add: change ];
				onDeactivation: [ :change | selectedChanges remove: change ];
				beNotExpandable);
			addColumn: (SpStringTableColumn evaluated: #name);
			yourself);
	yourself.

	scopeLabel := self newLabel label: 'Scope '.
	scopeDropList := self newDropList.
	scopeDropList display: [ :scope | scope description ].
	
	buttonOk := self newButton
		label: 'Ok';
		icon: (self iconNamed: #smallOk).

	buttonCancel := self newButton
		label: 'Cancel';
		icon: (self iconNamed: #smallCancel) .
		
	newNameLabel := self newLabel label: 'Selector'.
	newNameInput := self newTextInput.
	
	validationLabel := self newLabel label: ''
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> name [ 
	^ name
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> name: aName [
	
	name := aName.
	newNameInput text: aName
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> newNameInput [

	^ newNameInput
]

{ #category : 'action' }
StRenameClassPreviewPresenter >> performChanges [
	"the following behavior should not be kept in the UI. 
	It should be moved to the driver or the refactoring itself because the driver is a model of interaction.
	May be this should be in the refactoring."

	[ ReChangeManager instance
			performChanges: selectedChanges ]
		asJob
		title: 'Refactoring';
		run
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> refactoring: aRBRemoveMethodDriver [ 
	refactoring := aRBRemoveMethodDriver
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> schedulePreviewUpdate: aString [

	previewUpdateDelay ifNotNil: [ previewUpdateDelay unschedule ].
	
	previewUpdateDelay := Delay forMilliseconds: 300.
	
	[ previewUpdateDelay wait. 
		self validateName: aString.
		self updateName  ] fork
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> scopes: aCollection [

	scopes := aCollection .
	self updateScopeList
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> selectorInput [

	^ newNameInput
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateChanges [

	changes ifNil: [ changesInScope := OrderedCollection new. 
		selectedChanges := OrderedCollection new ] 

	ifNotNil: [ 
		changesInScope := changes select: [ :change |
							change isInScope: activeScope ].
		selectedChanges := OrderedCollection withAll: changesInScope ]
	
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateGlobalActiveScope [

	ScopesManager activeScope: scopeDropList selectedItem asRBEnvironment
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateName [

	name := newNameInput text.
	
	nameIsValid 
		ifFalse: [ 
			changes := nil.
			compositeChange := nil.
			self clearDiff ]
		
		ifTrue: [ 
			compositeChange := driver refactoring generateChanges .
			changes := compositeChange changesToDisplayIn: self ] .
		
	self updateChanges .
	self updateTablePresenter
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateScopeList [

	scopeDropList items: scopes.
	self globalActiveScope 
		ifNotNil: [ :scope | scopeDropList selectItem: scope ]
		ifNil: [ scopeDropList selectFirst ]
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> validateName: aString [
	
	| message |
	
	driver newName: aString.
	driver createAndSetNewRefactoring.
	message := driver checkApplicabilityPreconditions.
	
	message 
		ifNil: [ 
			nameIsValid := true.
			validationLabel label: '' ]
		ifNotNil: [ 
			nameIsValid := false.
			validationLabel label: 
				(message asText
            		addAttribute: (TextColor color:
                	(Color r: 1.0 g: 0.5 b: 0.31));
            		yourself) ]
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> validationLabel [
	^ validationLabel
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> windowTitle [ 
	
	^ 'Refactoring changes'
]
