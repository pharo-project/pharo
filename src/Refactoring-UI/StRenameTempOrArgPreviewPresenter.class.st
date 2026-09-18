"
I am a presenter for renaming an argument or a temporary variable. I allow the user to enter a new name, choose the refactoring scope, and inspect the resulting changes. The preview is updated live as the user edits the new name. The Apply button is enabled only when the name is valid and the refactoring can be applied.

I am responsible for keeping track of the selected changes. When the user selects a change, I display a diff between its old and new versions.

When renaming a temporary variable may change the behavior of the code, I inform the user and allow them to choose whether to apply the rename anyway or apply the suggested transformations. When renaming an argument, I display a message to inform the user about the potential issue.

"
Class {
	#name : 'StRenameTempOrArgPreviewPresenter',
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
		'nameIsValid',
		'behaviorOptionsLayout',
		'behaviorOptionsPanel',
		'updateProcess'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'instance creation' }
StRenameTempOrArgPreviewPresenter class >> withChanges: changes scopes: scopes [
	
	^ self new
		  changesFrom: changes scopes: scopes;
		  yourself
]

{ #category : 'action' }
StRenameTempOrArgPreviewPresenter >> accept [

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
StRenameTempOrArgPreviewPresenter >> addBehaviorPanel [

	behaviorMessageLabel label: driver checkBehaviorPreservingPreconditions.
	
	driver isTemp ifTrue: [ 
		(behaviorLayout children includes: behaviorOptionsPanel) 
			ifFalse: [ behaviorLayout add: behaviorOptionsPanel expand: false ]
	].
	
	(topLayout children includes: behaviorPanel)
		ifFalse: [ topLayout add: behaviorPanel expand: false ] 
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> applyAnyway [

	driver createAndSetNewRefactoring.
	self updateName
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> applyTransformation [

	driver createAndSetNewRefactoring .
	compositeChange := driver generateRemoveVariableChanges.
	changes := compositeChange changesToDisplayIn: self.

	self updateChanges.
	self updateTablePresenter
]

{ #category : 'private' }
StRenameTempOrArgPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'private' }
StRenameTempOrArgPreviewPresenter >> buildLeftSideDiff [

	^ diffPresenter
		  leftText:  driver method sourceCode ;
		  rightText: ''
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> buttonOk [
	^ buttonOk 
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> buttonOk: aButton [
	buttonOk := aButton .
	self updateOkButtonState.
]

{ #category : 'initialization' }
StRenameTempOrArgPreviewPresenter >> changesFrom: aCompositeChange [

	compositeChange := aCompositeChange. 
	"we keep the composite because we want to use it for performing changes later on."

	changes := aCompositeChange changesToDisplayIn: self.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'initialization' }
StRenameTempOrArgPreviewPresenter >> changesFrom: aCompositeChange scopes: aCollection [

	scopes := aCollection.
	self changesFrom: aCompositeChange.
]

{ #category : 'action' }
StRenameTempOrArgPreviewPresenter >> closeWindow [

	self withWindowDo: [ :window | window close ]
]

{ #category : 'initialization' }
StRenameTempOrArgPreviewPresenter >> connectPresenters [

	table whenSelectionChangedDo: [ :selection | selection selectedItem ifNotNil: [ self buildDiffFor: selection selectedItem ] ].

	scopeDropList whenSelectedItemChangedDo: [ :scope |
			self
				updateActiveScope;
				updateChanges;
				updateTablePresenter ].

	buttonCancel action: [ self closeWindow ].
	buttonOk action: [ self accept ].
		
	newNameInput whenTextChangedDo: [ :text | self schedulePreviewUpdate: text ].
	
	newNameInput whenSubmitDo: [ :text | self accept ].

	buttonApplyAnyway  whenActivatedDo: [ self applyAnyway ];
							associatedRadioButtons: { buttonApplyTransformation }.
	buttonApplyTransformation whenActivatedDo: [ self applyTransformation ];
									  associatedRadioButtons: { buttonApplyAnyway }.
	buttonApplyAnyway state: true.
	buttonApplyTransformation state: false
]

{ #category : 'api - focus' }
StRenameTempOrArgPreviewPresenter >> defaultKeyboardFocus [

	^ buttonOk
]

{ #category : 'layout' }
StRenameTempOrArgPreviewPresenter >> defaultLayout [

	| scopesLayout nameLayout inputLayout behaviorHeaderLayout |
	
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
		                        spacing: 3;
		                        add: behaviorWarningIcon expand: false;
		                        add: behaviorWarningTitle expand: false;
		                        yourself.

	behaviorOptionsLayout := SpBoxLayout newTopToBottom
		                         spacing: 2;
		                         add: buttonApplyAnyway expand: false;
		                         add: buttonApplyTransformation expand: false;
		                         yourself.
	behaviorOptionsPanel layout: behaviorOptionsLayout.

	behaviorLayout := SpBoxLayout newTopToBottom
		                  spacing: 2;
		                  add: behaviorHeaderLayout;
		                  add: behaviorMessageLabel expand: false;
		                  yourself. 
	behaviorPanel layout: behaviorLayout.

	topLayout := SpBoxLayout newTopToBottom
		             spacing: 5;
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
StRenameTempOrArgPreviewPresenter >> driver: aDriver [

   driver := aDriver
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> hasWarning [

	^ driver notNil and: [ driver hasFailedBehaviorPreservingPreconditions ]
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> hideBehaviorPanel [

	(topLayout children includes: behaviorPanel)
		ifFalse: [ ^ self].
		
	topLayout remove: behaviorPanel.
			self rebuildLayout
]

{ #category : 'initialization' }
StRenameTempOrArgPreviewPresenter >> initializeDialogWindow: aDialogWindowPresenter [

	| applyButton |
	aDialogWindowPresenter addButton: 'Cancel' do: [ :presenter |
			self terminateUpdateProcess.
			presenter close ].

	applyButton := aDialogWindowPresenter addDefaultButton: 'Apply' do: [ :presenter |
			               self terminateUpdateProcess.
			               self accept.
			               presenter close ].

	self buttonOk: applyButton
]

{ #category : 'initialization' }
StRenameTempOrArgPreviewPresenter >> initializePresenters [

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
	behaviorOptionsPanel := SpPresenter new.
	nameIsValid := false.

	buttonApplyAnyway := self instantiate: SpRadioButtonPresenter.
	buttonApplyAnyway label: 'Apply anyway'.

	buttonApplyTransformation := self instantiate: SpRadioButtonPresenter.
	buttonApplyTransformation label: 'Migrate it to existing instance variable'
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> name [

	^ name
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> name: aName [
    name := aName.
    newNameInput text: aName
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> newNameInput [

    ^ newNameInput
]

{ #category : 'action' }
StRenameTempOrArgPreviewPresenter >> performChanges [
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
StRenameTempOrArgPreviewPresenter >> refactoring: aRenameArgOrTempRefactoring [ 
	refactoring := aRenameArgOrTempRefactoring
	
	
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> schedulePreviewUpdate: aString [
	"Use a short delay to avoid updating the preview on every keystroke while keeping the UI responsive."

	updateProcess ifNotNil: [ updateProcess terminate ].

	updateProcess := [
		                 200 milliSeconds wait.
		                 self application defer: [
				                 self validateName: aString.
				                 self updateBehaviorPanelVisibility: aString.
				                 self updateName ] ] fork
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> scopes: aCollection [

	scopes := aCollection.
	self updateScopeList
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> selectorInput [
    ^ newNameInput
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> terminateUpdateProcess [

	updateProcess ifNotNil: [ 
		updateProcess terminate.
		updateProcess := nil ]
]

{ #category : 'update' }
StRenameTempOrArgPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> updateBehaviorPanelVisibility: aString [

	mainLayout ifNil: [ ^ self ].

	driver newName: aString.
	
	self hasWarning
		ifTrue: [ self addBehaviorPanel ]
		ifFalse: [ self hideBehaviorPanel ]
		
]

{ #category : 'update' }
StRenameTempOrArgPreviewPresenter >> updateChanges [

	changes ifNil: [ changesInScope := OrderedCollection new.
		selectedChanges := OrderedCollection new   ]
	ifNotNil: [  
	changesInScope := changes select: [ :change |
							change isInScope: activeScope ].
	selectedChanges := OrderedCollection withAll: changesInScope]
]

{ #category : 'update' }
StRenameTempOrArgPreviewPresenter >> updateGlobalActiveScope [

	ScopesManager activeScope: scopeDropList selectedItem asRBEnvironment
]

{ #category : 'update' }
StRenameTempOrArgPreviewPresenter >> updateName [

	name := newNameInput text.
	
	nameIsValid
		ifFalse: [
			compositeChange := nil.
			changes := nil.
			self buildLeftSideDiff .
			buttonOk disable ]
		ifTrue: [
			compositeChange := driver refactoring generateChanges.
			changes := compositeChange changesToDisplayIn: self .
			buttonOk enable ].
		
	self updateChanges .
	self updateTablePresenter
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> updateOkButtonState [

	nameIsValid
		ifTrue: [ buttonOk enable ]
		ifFalse: [ buttonOk disable ]
]

{ #category : 'update' }
StRenameTempOrArgPreviewPresenter >> updateScopeList [

	scopeDropList items: scopes.
	self globalActiveScope 
		ifNotNil: [ :scope | scopeDropList selectItem: scope ]
		ifNil: [ scopeDropList selectFirst ]
]

{ #category : 'update' }
StRenameTempOrArgPreviewPresenter >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> validateName: aString [

	| newName |

	driver newName: aString.
	driver createAndSetNewRefactoring.
	newName := driver checkApplicabilityPreconditions.

	newName
		ifNil: [
				nameIsValid := true.
				validationLabel label: '' ]
		ifNotNil: [
				nameIsValid := false.
				validationLabel label: (newName asText
						 addAttribute: (TextColor color: Smalltalk ui theme dangerTextColor);
						 yourself) ]
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> validationLabel [

	^ validationLabel
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenter >> windowTitle [ 
	
	^ 'Refactoring changes'
]
