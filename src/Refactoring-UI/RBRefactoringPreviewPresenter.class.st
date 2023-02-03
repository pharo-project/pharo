"
I am a tool that previews refactoring changes to the user. User can change the scope of the refactoring (default scopes are: class, hierarchy, package and image) and manually pick which changes to apply and which ones to skip. I am usually opened by `RBDriver` subclasses.

I am responsible for keeping track of `selectedChanges` (which changes to apply). When user selects a change from the list of all changes, I display a diff between old and new version of that change. When user accepts changes he wants to execute, I am responsible for invoking `RBRefactoryChangeManager` to perform them.

"
Class {
	#name : #RBRefactoringPreviewPresenter,
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
	#category : #'Refactoring-UI'
}

{ #category : #'instance creation' }
RBRefactoringPreviewPresenter class >> changes: aCompositeRefactoring inEnvironment: aRBNamespace scopes: scopes [

	^ self new
		  changesFrom: aCompositeRefactoring inEnvironment: aRBNamespace scopes: scopes;
		  yourself
]

{ #category : #specs }
RBRefactoringPreviewPresenter class >> title [
	^ 'Refactoring changes'
]

{ #category : #action }
RBRefactoringPreviewPresenter >> accept [

	(self okToChange not or: [ selectedChanges isEmptyOrNil ]) ifTrue: [
		UIManager default inform: 'No changes to apply.'.
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
RBRefactoringPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : #initialization }
RBRefactoringPreviewPresenter >> changesFrom: aCompositeChange inEnvironment: anEnvironment scopes: aCollection [

	changes := aCompositeChange whatToDisplayIn: self.
	scopes := aCollection.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : #action }
RBRefactoringPreviewPresenter >> closeWindow [

	self window close
]

{ #category : #initialization }
RBRefactoringPreviewPresenter >> columns [

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
RBRefactoringPreviewPresenter >> connectPresenters [

	super connectPresenters.
	table whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [
			self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope | self updateActiveScope; updateChanges ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ]
]

{ #category : #initialization }
RBRefactoringPreviewPresenter >> initializeLayout [

	| scopesLayout actionsLayout |
	scopesLayout := SpBoxLayout newLeftToRight
		                add: scopeLabel
		                withConstraints:
			                [ :constraints | constraints width: 60 ] yourself;
		                add: scopeDropList;
		                yourself.
	actionsLayout := SpBoxLayout newLeftToRight
		                 addLast: buttonOk
		                 expand: false
		                 fill: true
		                 padding: 5;
		                 addLast: buttonCancel
		                 expand: false
		                 fill: true
		                 padding: 5;
		                 yourself.

	self layout: (SpBoxLayout newTopToBottom
			 add: (SpBoxLayout newTopToBottom
					  add: scopesLayout
					  withConstraints:
						  [ :constraints | constraints height: 30 ] yourself;
					  add: table;
					  add: diffPresenter;
					  yourself);
			 add: actionsLayout expand: false;
			 yourself)
]

{ #category : #initialization }
RBRefactoringPreviewPresenter >> initializePresenters [

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
		add: buttonCancel.
	self initializeLayout
]

{ #category : #initialization }
RBRefactoringPreviewPresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self title;
		initialExtent: 700 @ 500
]

{ #category : #update }
RBRefactoringPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : #update }
RBRefactoringPreviewPresenter >> updateChanges [

	changesInScope := changes select: [ :change |
		                  activeScope includesClass: change changeClass ].
	selectedChanges := OrderedCollection withAll: changesInScope
]

{ #category : #update }
RBRefactoringPreviewPresenter >> updateScopeList [

	scopeDropList items: scopes
]

{ #category : #update }
RBRefactoringPreviewPresenter >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]
