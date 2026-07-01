"
I am a tool that previews refactoring changes to the user. User can change the scope of the refactoring (default scopes are: class, hierarchy, package and image) and manually pick which changes to apply and which ones to skip. I am usually opened by `RBDriver` subclasses.

I am responsible for keeping track of `selectedChanges` (which changes to apply). When user selects a change from the list of all changes, I display a diff between old and new version of that change. When user accepts changes he wants to execute, I am responsible for invoking `RBRefactoryChangeManager` to perform them.

"
Class {
	#name : 'StRefactoringPreviewPresenter2',
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
		'buttonUpdate',
		'name',
		'newNameLabel',
		'newNameInput',
		'driver',
		'validationLabel'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'instance creation' }
StRefactoringPreviewPresenter2 class >> withChanges: changes scopes: scopes [
	
	^ self new
		  changesFrom: changes scopes: scopes;
		  yourself
]

{ #category : 'action' }
StRefactoringPreviewPresenter2 >> accept [

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
StRefactoringPreviewPresenter2 >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> changesFrom: aCompositeChange [

	compositeChange := aCompositeChange.
	"we keep the composite because we want to use it for performing changes later on."
	
	changes := aCompositeChange changesToDisplayIn: self.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> changesFrom: aCompositeChange scopes: aCollection [
self halt.
	scopes := aCollection.
	self changesFrom: aCompositeChange.
]

{ #category : 'action' }
StRefactoringPreviewPresenter2 >> closeWindow [

	self withWindowDo: [ :window | window close ]
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> connectPresenters [

	table whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [
			self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope | 
			self 
				updateActiveScope; 
				updateChanges; 
				updateTablePresenter ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ].
	buttonUpdate action: [ self updateChangesWithName ].
]

{ #category : 'api - focus' }
StRefactoringPreviewPresenter2 >> defaultKeyboardFocus [

	^ buttonOk
]

{ #category : 'layout' }
StRefactoringPreviewPresenter2 >> defaultLayout [

	| scopesLayout nameLayout inputLayout selectorLayout |
	scopesLayout := SpBoxLayout newLeftToRight
		                vAlignCenter;
		                spacing: 5;
		                add: scopeLabel expand: false;
		                add: scopeDropList;
		                yourself.

	inputLayout := SpBoxLayout newTopToBottom
		               spacing: 2;
		               add: newNameInput expand: false;
		               add: validationLabel height: 90;
		               yourself.
	nameLayout := SpBoxLayout newLeftToRight
		               spacing: 5;
		               add: newNameLabel expand: false;
		               add: inputLayout;
		               add: buttonUpdate expand: false;
		               yourself.

	selectorLayout := SpGridLayout new
		                  borderWidth: 0;
		                  beColumnNotHomogeneous;
		                  row: 2 expand: true;
		                  column: 2 expand: true;
		                  rowSpacing: 4;
		                  add: newNameLabel atPoint: 1 @ 1;
		                  add: newNameInput atPoint: 2 @ 1;
		                  add: buttonUpdate atPoint: 3 @ 1;
		                  add: validationLabel atPoint: 2 @ 2;
		                  yourself.
	^ SpBoxLayout newTopToBottom
		  spacing: 10;
		  add: selectorLayout expand: true;
		  add: scopesLayout expand: false;
		  add: table;
		  add: diffPresenter;
		  yourself
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> driver: aDriver [

   driver := aDriver 
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> initializeDialogWindow: aDialogWindowPresenter [
	"Used to initialize the model in the case of the use into a dialog window.
	 Override this to set buttons other than the default (Ok, Cancel)."
	
	aDialogWindowPresenter
		addButton: 'Cancel' do: [ :presenter | presenter close ];
		addDefaultButton: 'Apply' do: [ :presenter | 
			self accept.
			presenter close ]
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> initializePresenters [

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
		label: 'Apply';
		icon: (self iconNamed: #smallOk).

	buttonCancel := self newButton
		label: 'Cancel';
		icon: (self iconNamed: #smallCancel).
		
	newNameLabel := self newLabel 
		label: 'Selector'.
	newNameInput := self newTextInput.
	newNameInput whenTextChangedDo: [ :text |
    self validateName: text ].
	buttonUpdate := self newButton
   		 label: 'Rename' .
		
	validationLabel := self newLabel .
	validationLabel label: ''
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> name [

	^ name
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> name: aName [
    name := aName.
    newNameInput text: aName
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> newNameInput [

    ^ newNameInput
]

{ #category : 'action' }
StRefactoringPreviewPresenter2 >> performChanges [
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
StRefactoringPreviewPresenter2 >> refactoring: aRBRemoveMethodDriver [ 
	refactoring := aRBRemoveMethodDriver
	
	
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> removeAllChanges [
"self halt."
	changes ifNotNil: [  
		changesInScope removeAll.
		selectedChanges removeAll.
	].
	
   changes := OrderedCollection new
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> scopes: aCollection [

	scopes := aCollection.
	self updateScopeList
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> selectorInput [
    ^ newNameInput
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateChanges [
"self halt."
	changes ifNil: [ changesInScope := OrderedCollection new.
		selectedChanges := OrderedCollection new   ]
	ifNotNil: [  
	"changesInScope removeAll.
	selectedChanges removeAll."
	changesInScope := changes select: [ :change |
							change isInScope: activeScope ].
	selectedChanges := OrderedCollection withAll: changesInScope]
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateChangesWithName [

	driver createAndSetNewRefactoring .
	self updateName .
	compositeChange:= driver changes .
	"we keep the composite because we want to use it for performing changes later on."
	changes := compositeChange changesToDisplayIn: self.
	self updateChanges.
	self updateTablePresenter.
	driver updateOldName 

	

	
	
	
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateGlobalActiveScope [

	ScopesManager activeScope: scopeDropList selectedItem asRBEnvironment
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> updateName [

	 name:= newNameInput text.
	 driver newName: name.


	 

	 
	
    
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateScopeList [

	scopeDropList items: scopes.
	self globalActiveScope 
		ifNotNil: [ :scope | scopeDropList selectItem: scope ]
		ifNil: [ scopeDropList selectFirst ]
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> validateName: aString [

    | message |

    driver newName: aString.
    message := driver checkApplicabilityPreconditions.

    message ifNil: [
    	validationLabel label: ''.
		buttonUpdate enable .
	] ifNotNil: [
   		 validationLabel label: 
        (message asText
            addAttribute: (TextColor color:
                (Color r: 1.0 g: 0.5 b: 0.31));
            yourself).
		 buttonUpdate disable .
		].
	
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> windowTitle [ 
	
	^ 'Refactoring changes'
]
