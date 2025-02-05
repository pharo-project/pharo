"
I am a tool that previews refactoring changes to the user. User can change the scope of the refactoring (default scopes are: class, hierarchy, package and image) and manually pick which changes to apply and which ones to skip. I am usually opened by `RBDriver` subclasses.

I am responsible for keeping track of `selectedChanges` (which changes to apply). When user selects a change from the list of all changes, I display a diff between old and new version of that change. When user accepts changes he wants to execute, I am responsible for invoking `RBRefactoryChangeManager` to perform them.

"
Class {
	#name : 'StRefactoringPreviewPresenter',
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
		'refactoring'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'instance creation' }
StRefactoringPreviewPresenter class >> changes: aCompositeRefactoring [
	self deprecated: 'Use `for:` instead.' 
		on: '2024/01/20' 
		in: 'pharo13' 
		transformWith: '`@receiver changes: `@arg' -> '`@receiver for: `@arg'.
	
	^ self new
		  changesFrom: aCompositeRefactoring scopes: { RBBrowserEnvironment new };
		  yourself
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenter class >> for: aCompositeRefactoring [
	
	^ self new
		  changesFrom: aCompositeRefactoring scopes: { RBBrowserEnvironment new };
		  yourself
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenter class >> for: aCompositeRefactoring scopes: scopes [
	self deprecated: 'Use `for:scopes:` instead.' 
		on: '2024-01-20' 
		in: 'pharo13' 
		transformWith: '`@receiver changes: `@arg1 scopes: `@arg2' 
			-> '`@receiver for: `@arg1 scopes: `@arg2'.

	^ self new
		  changesFrom: aCompositeRefactoring scopes: scopes;
		  yourself
]

{ #category : 'action' }
StRefactoringPreviewPresenter >> accept [
	
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
StRefactoringPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter >> changesFrom: aCompositeChange [

	compositeChange := aCompositeChange.
	"we keep the composite because we want to use it for performing changes later on."
	
	changes := aCompositeChange whatToDisplayIn: self.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter >> changesFrom: aCompositeChange scopes: aCollection [

	scopes := aCollection.
	self changesFrom: aCompositeChange.
]

{ #category : 'action' }
StRefactoringPreviewPresenter >> closeWindow [

	self window close
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter >> columns [

	^ { (SpCompositeTableColumn new
		   addColumn:
			   ((SpCheckBoxTableColumn evaluated: [ :change | selectedChanges includes: change ])
				    onActivation: [ :change | selectedChanges add: change ];
				    onDeactivation: [ :change | selectedChanges remove: change ];
				    width: 20);
		   addColumn: (SpStringTableColumn evaluated: #name);
		   yourself) }
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter >> connectPresenters [

	table whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [
			self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope | 
			self 
				updateActiveScope; 
				updateChanges; 
				updateTablePresenter ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ]
]

{ #category : 'layout' }
StRefactoringPreviewPresenter >> defaultLayout [
	| scopesLayout |
	
	scopesLayout := SpBoxLayout newLeftToRight
		vAlignCenter;
		spacing: 5;
		add: scopeLabel expand: false;
		add: scopeDropList;
		yourself.
	
	^ SpBoxLayout newTopToBottom
		spacing: 5;
		add: scopesLayout expand: false;
		add: table;
		add: diffPresenter;
		yourself
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter >> initializeDialogWindow: aDialogWindowPresenter [
	"Used to initialize the model in the case of the use into a dialog window.
	 Override this to set buttons other than the default (Ok, Cancel)."
	
	aDialogWindowPresenter
		addButton: 'Cancel' do: [ :presenter | presenter close ];
		addDefaultButton: 'Ok' do: [ :presenter | 
			self accept.
			presenter close ]
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter >> initializePresenters [

	diffPresenter := self newDiff.
	diffPresenter disable.

	table := self newTable.
	table
		columns: self columns;
		hideColumnHeaders.

	scopeLabel := self newLabel label: 'Scope '.
	scopeDropList := self newDropList.
	scopeDropList display: [ :scope | scope description ].
	
	buttonOk := self newButton
		            label: 'Ok';
		            icon: (self iconNamed: #smallOk).

	buttonCancel := self newButton
		                label: 'Cancel';
		                icon: (self iconNamed: #smallCancel).
	self focusOrder
		add: buttonOk;
		add: buttonCancel
]

{ #category : 'action' }
StRefactoringPreviewPresenter >> performChanges [
	"the following behavior should not be kept in the UI. 
	It should be moved to the driver or the refactoring itself because the driver is a model of interaction.
	May be this should be in the refactoring."

	[ RBRefactoryChangeManager instance
			performChanges: selectedChanges ]
		asJob
		title: 'Refactoring';
		run
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter >> refactoring: aRBRemoveMethodDriver [ 
	refactoring := aRBRemoveMethodDriver
]

{ #category : 'update' }
StRefactoringPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : 'update' }
StRefactoringPreviewPresenter >> updateChanges [

	changesInScope := changes select: [ :change |
							change isInScope: activeScope ].
	selectedChanges := OrderedCollection withAll: changesInScope
]

{ #category : 'update' }
StRefactoringPreviewPresenter >> updateGlobalActiveScope [

	ScopesManager activeScope: scopeDropList selectedItem asRBEnvironment
]

{ #category : 'update' }
StRefactoringPreviewPresenter >> updateScopeList [

	scopeDropList items: scopes.
	self globalActiveScope ifNotNil: [ :scope | scopeDropList selectItem: scope ]
]

{ #category : 'update' }
StRefactoringPreviewPresenter >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter >> windowTitle [ 
	
	^ 'Refactoring changes'
]
