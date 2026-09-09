"
I am a tool for previewing the changes produced by a class rename refactoring. I allow the user to enter a new name, choose the refactoring scope, and inspect the resulting changes. The preview is updated live as the user edits the new name and enables the Apply button only when the name is valid and the refactoring can be applied.

I am responsible for keeping track of the selected changes. When the user selects a change, I display a diff between its old and new versions. When behavior-preserving preconditions are detected, I display the corresponding warning and provide the user with a way to browse the affected methods.
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
		'topLayout',
		'behaviorWarningIcon',
		'behaviorLayout',
		'behaviorPanel',
		'behaviorWarningTitle',
		'behaviorMessageLabel',
		'buttonBrowseMethod'
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

{ #category : 'action' }
StRenameClassPreviewPresenter >> addBehaviorPanel [

	behaviorMessageLabel label: driver checkBehaviorPreconditions.
	
	(topLayout children includes: behaviorPanel)
		ifFalse: [ topLayout add: behaviorPanel expand: false ]
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> behaviorPanel [
	^ behaviorPanel
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> browseMethods [

	 driver browseMethods
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> buttonOk [
	^ buttonOk
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> buttonOk: aButton [

	buttonOk := aButton.
	self updateOkButtonState
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> changes [
 	^ changes
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
		leftText: (driver refactoring class realClass) definitionString;
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
	buttonBrowseMethod action: [ self browseMethods ].
	
	newNameInput whenTextChangedDo: [ :text |
		self schedulePreviewUpdate: text ] 
]

{ #category : 'api - focus' }
StRenameClassPreviewPresenter >> defaultKeyboardFocus [

	^ buttonOk
]

{ #category : 'layout' }
StRenameClassPreviewPresenter >> defaultLayout [
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
		add:newNameLabel expand: false; 
		add: inputLayout;
		yourself.
		
	topLayout := SpBoxLayout newTopToBottom 
		spacing: 10;
		add: nameLayout expand: false;
		yourself.
		
	behaviorHeaderLayout := SpBoxLayout newLeftToRight 
		vAlignCenter;
		spacing: 5;
		add: behaviorWarningIcon expand: false;
		add: behaviorWarningTitle expand: false;
		yourself.
		
	behaviorLayout := SpBoxLayout newTopToBottom
		spacing: 4;
		add: behaviorHeaderLayout;
		add: behaviorMessageLabel expand: false;
		add: buttonBrowseMethod expand: false;
		yourself.
	behaviorPanel layout: behaviorLayout.
	
	mainLayout := SpBoxLayout newTopToBottom
		spacing: 10;
		add: topLayout expand: false;
		add: scopesLayout expand: false;
		add: table;
		add: diffPresenter;
		yourself.

	^ mainLayout
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> driver: aDriver [
	driver := aDriver
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'action' }
StRenameClassPreviewPresenter >> hasWarning [

	^ driver notNil and: [ driver hasFailedBehaviorPreservingPreconditions ]
]

{ #category : 'action' }
StRenameClassPreviewPresenter >> hideBehaviorPanel [
	
	(topLayout children includes: behaviorPanel)
		ifFalse: [ ^ self ].
		
	topLayout remove: behaviorPanel.
		self rebuildLayout 
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> initializeDialogWindow: aDialogWindowPresenter [

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
		label: 'Accept';
		icon: (self iconNamed: #smallOk).

	buttonCancel := self newButton
		label: 'Cancel';
		icon: (self iconNamed: #smallCancel) .
		
	newNameLabel := self newLabel label: 'Selector'.
	newNameInput := self newTextInput.
	
	validationLabel := self newLabel label: '' .
	
	behaviorWarningIcon := self newImage image: (self iconNamed: #warningIcon) .
	behaviorWarningTitle := self newLabel label: 'Behavior-preserving warnings' .
	behaviorMessageLabel := self newLabel label: ''.
	behaviorPanel := SpPresenter new .
	nameIsValid := false. 
	
	buttonBrowseMethod := self newButton
		label: 'Browse affected methods';
		icon: (self iconNamed: #search)
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
	
	previewUpdateDelay := Delay forMilliseconds: 250.
	
	[ previewUpdateDelay wait. 
		self validateName: aString.
		self updateBehaviorPanelVisibility: aString.
		self updateName ] fork
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

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> topLayout [
	^ topLayout
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : 'action' }
StRenameClassPreviewPresenter >> updateBehaviorPanelVisibility: aString [

	mainLayout ifNil: [ ^ self ].
	
	driver newName: aString .
	
	self hasWarning 
		ifTrue: [ self addBehaviorPanel ]
		ifFalse: [ self hideBehaviorPanel ]
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
			self clearDiff .
			buttonOk disable]
		
		ifTrue: [ 
			compositeChange := driver refactoring privateTransform .
			changes := compositeChange changesToDisplayIn: self .
			buttonOk enable] .
		
	self updateChanges .
	self updateTablePresenter
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> updateOkButtonState [

	nameIsValid
		ifTrue: [ buttonOk enable ]
		ifFalse: [ buttonOk disable ]
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
