"
I am a tool that previews refactoring changes to the user. User can change the scope of the refactoring (default scopes are: class, hierarchy, package and image) and manually pick which changes to apply and which ones to skip. I am usually opened by `RBDriver` subclasses.

I am responsible for keeping track of `selectedChanges` (which changes to apply). When user selects a change from the list of all changes, I display a diff between old and new version of that change. When user accepts changes he wants to execute, I am responsible for invoking `RBRefactoryChangeManager` to perform them.

"
Class {
	#name : #StRefactoringPreviewPresenter,
	#superclass : #SpPresenter,
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
		'buttonCancel'
	],
	#category : #'Refactoring-UI-UI'
}

{ #category : #'instance creation' }
StRefactoringPreviewPresenter class >> changes: aCompositeRefactoring scopes: scopes [

	^ self new
		  changesFrom: aCompositeRefactoring scopes: scopes;
		  yourself
]

{ #category : #specs }
StRefactoringPreviewPresenter class >> title [
	^ 'Refactoring changes'
]

{ #category : #action }
StRefactoringPreviewPresenter >> accept [
	
	(self okToChange not or: [ selectedChanges isEmptyOrNil ]) ifTrue: [
		"UIManager default inform: 'No changes to apply.'."
		self application newInform 
				title: 'No changes to apply.';
				acceptLabel: 'Close this!';
				openDialog.
		
		self closeWindow ].
	[
	selectedChanges do: [ :change |
			RBRefactoryChangeManager instance
				performChange: change;
				addUndoPointer: RBRefactoryChangeManager nextCounter ] ] asJob
		title: 'Refactoring';
		run.
	self closeWindow
]

{ #category : #private }
StRefactoringPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : #initialization }
StRefactoringPreviewPresenter >> changesFrom: aCompositeChange scopes: aCollection [

	changes := aCompositeChange collect: [ :each | each whatToDisplayIn: self].
	changes := changes flattened.
	scopes := aCollection.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : #action }
StRefactoringPreviewPresenter >> closeWindow [

	self window close
]

{ #category : #initialization }
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

{ #category : #initialization }
StRefactoringPreviewPresenter >> connectPresenters [

	table whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [
			self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope | self updateActiveScope; updateChanges ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ]
]

{ #category : #layout }
StRefactoringPreviewPresenter >> defaultLayout [

	| scopesLayout actionsLayout |
	scopesLayout := SpBoxLayout newLeftToRight
		                add: scopeLabel
		                withConstraints:
			                [ :constraints | constraints width: 60 ] yourself;
		                add: scopeDropList;
		                yourself.
	actionsLayout := SpBoxLayout newLeftToRight
		                 spacing: 5;
		                 addLast: buttonOk expand: false fill: true;
		                 addLast: buttonCancel expand: false fill: true;
		                 yourself.

	^ SpBoxLayout newTopToBottom
		  add: (SpBoxLayout newTopToBottom
				   add: scopesLayout
				   withConstraints:
					   [ :constraints | constraints height: 30 ] yourself;
				   add: table;
				   add: diffPresenter;
				   yourself);
		  add: actionsLayout expand: false;
		  yourself
]

{ #category : #initialization }
StRefactoringPreviewPresenter >> initializePresenters [

	diffPresenter := self newDiff.
	diffPresenter disable.

	table := self newTable.
	table
		columns: self columns;
		hideColumnHeaders.

	scopeLabel := self newLabel label: 'Scope '.
	scopeDropList := self newDropList.
	scopeDropList display: [ :scope | scope description capitalized ].

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

{ #category : #initialization }
StRefactoringPreviewPresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self title;
		initialExtent: 700 @ 500
]

{ #category : #update }
StRefactoringPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : #update }
StRefactoringPreviewPresenter >> updateChanges [

	changesInScope := changes select: [ :change |
		                  activeScope includesClass: change changeClass ].
	selectedChanges := OrderedCollection withAll: changesInScope
]

{ #category : #update }
StRefactoringPreviewPresenter >> updateScopeList [

	scopeDropList items: scopes
]

{ #category : #update }
StRefactoringPreviewPresenter >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]
