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
		'refactoring',
		'name',
		'newNameLabel',
		'newNameInput',
		'driver',
		'validationLabel',
		'nameIsValid',
		'mainLayout',
		'topLayout',
		'behaviorWarningIcon',
		'behaviorLayout',
		'behaviorPanel',
		'behaviorWarningTitle',
		'behaviorMessageLabel',
		'buttonBrowseMethod',
		'previewRequest',
		'previewRunning',
		'pendingPreview'
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

	(nameIsValid and: [ self isPreviewPending not and: [ selectedChanges isEmptyOrNil not ] ])
		ifFalse: [ ^ self ].
	self performChanges.
	self updateGlobalActiveScope.
	self closeWindow
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

{ #category : 'action' }
StRenameClassPreviewPresenter >> closeWindow [

	self invalidatePreview.

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

	newNameInput whenTextChangedDo: [ :text | self refreshPreview: text ] .
	newNameInput whenSubmitDo: [ :text | self accept ]
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
StRenameClassPreviewPresenter >> hideBehaviorPanel [

	topLayout ifNil: [ ^ self ].

	(topLayout children includes: behaviorPanel)
		ifFalse: [ ^ self ].

	topLayout remove: behaviorPanel.
		self rebuildLayout
]

{ #category : 'initialization' }
StRenameClassPreviewPresenter >> initializeDialogWindow: aDialogWindowPresenter [

	| applyButton |
	aDialogWindowPresenter whenClosedDo: [ self invalidatePreview ].
	aDialogWindowPresenter addButton: 'Cancel' do: [ :presenter |
			presenter close ].

	applyButton := aDialogWindowPresenter addDefaultButton: 'Apply' do: [ :presenter |
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
		label: 'Apply';
		icon: (self iconNamed: #smallOk).

	buttonCancel := self newButton
		label: 'Cancel';
		icon: (self iconNamed: #smallCancel) .

	newNameLabel := self newLabel label: 'New name'.
	newNameInput := self newTextInput.

	validationLabel := self newLabel
		label: '';
		addStyle: 'dim';
		yourself.

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
StRenameClassPreviewPresenter >> refactoring: aRenameClassRefactoring [
	refactoring := aRenameClassRefactoring
]

{ #category : 'update' }
StRenameClassPreviewPresenter >> refreshPreview: aString [

	| request previewDriver |
	self invalidatePreview.
	self clearPreview.
	(aString isEmpty or: [ aString = driver oldName asString ]) ifTrue: [ ^ self ].
	request := Object new.
	previewRequest := request.
	previewDriver := driver.
	validationLabel label: 'Preparing preview...'.
	pendingPreview := [
		previewRequest == request
		ifFalse: [ [ ] ]
		ifTrue: [
			| result |
			result := [
				previewDriver previewFor: aString
			] on: Error do: [ :error |
				ReClassRenamePreview new errorMessage: error messageText; yourself ].
			[ self publishPreview: result named: aString request: request ] ] ].
	self startNextPreview
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

{ #category : 'private' }
StRenameClassPreviewPresenter >> invalidatePreview [

	"Invalidate queued completions. Running computations own isolated models."
	previewRequest := nil.
	pendingPreview := nil
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> topLayout [
	^ topLayout
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
StRenameClassPreviewPresenter >> validationLabel [
	^ validationLabel
]

{ #category : 'accessing' }
StRenameClassPreviewPresenter >> windowTitle [

	^ 'Refactoring changes'
]

{ #category : 'testing' }
StRenameClassPreviewPresenter >> isPreviewPending [

	^ previewRequest notNil
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> clearPreview [

	nameIsValid := false.
	changes := nil.
	validationLabel label: ''.
	buttonOk disable.
	diffPresenter leftText: ''; rightText: ''.
	self hideBehaviorPanel.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> schedulePreview: aBlock [

	[ 150 milliSeconds wait. aBlock value ] forkAt: Processor userBackgroundPriority
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> dispatchPreview: aBlock [

	self application defer: aBlock
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> publishPreview: result named: aString request: request [

	previewRequest == request ifFalse: [ ^ self ].
	previewRequest := nil.
	[
		result errorMessage ifNotNil: [
			self clearPreview.
			validationLabel label: result errorMessage.
			^ self ].
		driver := result driver.
		name := aString.
		changes := result compositeChange changesToDisplayIn: self.
		result warning ifNotNil: [
			behaviorMessageLabel label: result warning.
			topLayout ifNotNil: [ topLayout add: behaviorPanel expand: false ] ].
		self updateChanges.
		self updateTablePresenter.
		validationLabel label: ''.
		nameIsValid := true.
		self updateOkButtonState
	] on: Error do: [ :error |
		self clearPreview.
		validationLabel label: error messageText
	]
]

{ #category : 'private' }
StRenameClassPreviewPresenter >> startNextPreview [

	| computation |
	(previewRunning == true or: [ pendingPreview isNil ]) ifTrue: [ ^ self ].
	computation := pendingPreview.
	pendingPreview := nil.
	previewRunning := true.
	self schedulePreview: [
		| publication |
		publication := computation value.
		self dispatchPreview: [
			[ publication value ] ensure: [
				previewRunning := false.
				self startNextPreview ] ] ]
]
