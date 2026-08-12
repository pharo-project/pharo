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
		'name',
		'newNameLabel',
		'newNameInput',
		'driver',
		'validationLabel',
		'behaviorWarningTitle',
		'behaviorMessageLabel',
		'buttonApplyAnyway',
		'behaviorWarningIcon',
		'buttonApplyTransformation',
		'applyAnyway',
		'mainLayout',
		'behaviorLayout',
		'behaviorPanel',
		'topLayout',
		'previewUpdateDelay',
		'nameIsValid'
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

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> applyTransformation [

	self updateName .
	driver createAndSetNewRefactoring .

	compositeChange := driver refactoring generateTransformChanges.
	driver prepareTransformation: driver refactoring parseTree.
	compositeChange := driver refactoring privateTransform.
	changes := compositeChange changesToDisplayIn: self.

	self updateChanges.
	self updateTablePresenter
]

{ #category : 'private' }
StRefactoringPreviewPresenter2 >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> buttonOk [
	^ buttonOk 
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> buttonOk: aButton [
	buttonOk := aButton 
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> changesFrom: aCompositeChange [

	compositeChange := aCompositeChange. "we keep the composite because we want to use it for performing changes later on."

	changes := aCompositeChange changesToDisplayIn: self.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> changesFrom: aCompositeChange scopes: aCollection [

	scopes := aCollection.
	self changesFrom: aCompositeChange.
]

{ #category : 'private' }
StRefactoringPreviewPresenter2 >> clearDiff [

    diffPresenter
        leftText: '';
        rightText: ''
]

{ #category : 'action' }
StRefactoringPreviewPresenter2 >> closeWindow [

	self withWindowDo: [ :window | window close ]
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> connectPresenters [

	table whenSelectionChangedDo: [ :selection | selection selectedItem ifNotNil: [ self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope |
			self
				updateActiveScope;
				updateChanges;
				updateTablePresenter ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ].

	newNameInput whenTextChangedDo: [ :text |
			self validateName: text.
			self updateBehaviorPanelVisibility: text.
			self schedulePreviewUpdate ].


	buttonApplyAnyway  whenActivatedDo: [ self updateChangesWithName ];
							associatedRadioButtons: { buttonApplyTransformation }.
	buttonApplyTransformation   whenActivatedDo: [ self applyTransformation ];
										associatedRadioButtons: { buttonApplyAnyway }.
	buttonApplyAnyway state: true
]

{ #category : 'api - focus' }
StRefactoringPreviewPresenter2 >> defaultKeyboardFocus [

	^ buttonOk
]

{ #category : 'layout' }
StRefactoringPreviewPresenter2 >> defaultLayout [

	| scopesLayout nameLayout inputLayout behaviorHeaderLayout behaviorOptionsLayout |
	
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
		              add: newNameLabel expand: false;
		              add: inputLayout;
		              yourself.

	behaviorHeaderLayout := SpBoxLayout newLeftToRight
		                        vAlignCenter;
		                        spacing: 5;
		                        add: behaviorWarningIcon expand: false;
		                        add: behaviorWarningTitle expand: false;
		                        yourself.

	behaviorOptionsLayout := SpBoxLayout newTopToBottom
		                         spacing: 2;
		                         add: buttonApplyAnyway expand: false;
		                         add: buttonApplyTransformation expand: false;
		                         yourself.

	behaviorLayout := SpBoxLayout newTopToBottom
		                  spacing: 4;
		                  add: behaviorHeaderLayout;
		                  add: behaviorMessageLabel expand: false;
		                  add: behaviorOptionsLayout;
		                  yourself. 
	behaviorPanel layout: behaviorLayout.

	topLayout := SpBoxLayout newTopToBottom
		             spacing: 10;
		             add: nameLayout expand: false;
		             yourself.


	mainLayout := SpBoxLayout newTopToBottom
		              spacing: 10;
		              add: topLayout expand: false;
		              add: scopesLayout expand: false;
		              add: table expand: true;
		              add: diffPresenter expand: true;
		              yourself.

	^ mainLayout
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> driver: aDriver [

   driver := aDriver
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> hasWarning [
	^ driver notNil and: [ driver hasFailedBehaviorPreservingPreconditions ]
]

{ #category : 'initialization' }
StRefactoringPreviewPresenter2 >> initializeDialogWindow: aDialogWindowPresenter [

	| applyButton |
	
	aDialogWindowPresenter
		addButton: 'Cancel' do: [ :presenter | presenter close ].

	applyButton := aDialogWindowPresenter
		addDefaultButton: 'Apply' do: [ :presenter | 
			self accept.
			presenter close ].

	self buttonOk: applyButton
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

	newNameLabel := self newLabel label: 'Selector'.

	newNameInput := self newTextInput.

	validationLabel := self newLabel label: ''.

	behaviorWarningTitle := self newLabel label: 'Behavior-preserving warnings'.
	behaviorWarningIcon := self newImage image: (self iconNamed: #warningIcon).

	behaviorMessageLabel := self newLabel label: ''.
	behaviorPanel := SpPresenter new.

	buttonApplyAnyway := self instantiate: SpRadioButtonPresenter.
	buttonApplyAnyway label: 'Apply anyway'.

	buttonApplyTransformation := self instantiate: SpRadioButtonPresenter.
	buttonApplyTransformation label: 'Migrate it to existing instance variable'
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

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> schedulePreviewUpdate [

	previewUpdateDelay ifNotNil: [ previewUpdateDelay unschedule ].

	previewUpdateDelay := Delay forMilliseconds: 350.

	[ previewUpdateDelay wait.
		self updateChangesWithName ] fork
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

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> updateBehaviorPanelVisibility: aString [

	mainLayout ifNil: [ ^ self ].

	driver newName: aString.
	
	self hasWarning
		ifTrue: [
			behaviorMessageLabel label: driver checkBehaviorPreservingPreconditions.
			self applyTransformation.
			
			(topLayout children includes: behaviorPanel)
				ifFalse: [ topLayout add: behaviorPanel expand: false ] ]
		ifFalse: [
			(topLayout children includes: behaviorPanel)
				ifTrue: [ 
					topLayout remove: behaviorPanel.
					self rebuildLayout ] ]
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateChanges [

	changes ifNil: [ changesInScope := OrderedCollection new.
		selectedChanges := OrderedCollection new   ]
	ifNotNil: [  
	changesInScope := changes select: [ :change |
							change isInScope: activeScope ].
	selectedChanges := OrderedCollection withAll: changesInScope]
]

{ #category : 'update' }
StRefactoringPreviewPresenter2 >> updateChangesWithName [

	nameIsValid ifFalse: [
			changes := nil.
			compositeChange := nil.
			self updateChanges.
			self updateTablePresenter.
			self clearDiff.
			^ self ].

	self updateName.
	driver createAndSetNewRefactoring.
	compositeChange := driver refactoring generateChanges.
	changes := compositeChange changesToDisplayIn: self.
	self updateChanges.
	self updateTablePresenter
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

	message
		ifNil: [
				nameIsValid := true.
				validationLabel label: '' ]
		ifNotNil: [
				nameIsValid := false.
				validationLabel label: (message asText
						 addAttribute: (TextColor color: (Color r: 1.0 g: 0.5 b: 0.31));
						 yourself) ]
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2 >> windowTitle [ 
	
	^ 'Refactoring changes'
]
