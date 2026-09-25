"
I am a tool that previews refactoring changes to the user. User can change the scope of the refactoring (default scopes are: class, hierarchy, package and image) and manually pick which changes to apply and which ones to skip. I am usually opened by `RBDriver` subclasses.

I am responsible for keeping track of `selectedChanges` (which changes to apply). When user selects a change from the list of all changes, I display a diff between old and new version of that change. When user accepts changes he wants to execute, I am responsible for invoking `RBRefactoryChangeManager` to perform them.

"
Class {
	#name : 'StRenameMethodPreviewPresenter',
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
		'driver',
		'name',
		'newNameLabel',
		'newNameInput',
		'validationLabel',
		'nameIsValid',
		'mainLayout',
		'topLayout',
		'argumentsList',
		'argLabel',
		'buttonUp',
		'buttonDown',
		'arguments',
		'updateProcess',
		'behaviorWarningIcon',
		'behaviorLayout',
		'behaviorPanel',
		'behaviorWarningTitle',
		'behaviorMessageLabel',
		'buttonBrowseMethod',
		'permutation'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'instance creation' }
StRenameMethodPreviewPresenter class >> withChanges: changes scopes: scopes [
	
	^ self new
		  changesFrom: changes scopes: scopes;
		  yourself
]

{ #category : 'action' }
StRenameMethodPreviewPresenter >> accept [
	
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

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> acceptValidName [

	nameIsValid := true.
	validationLabel label: ''
]

{ #category : 'action' }
StRenameMethodPreviewPresenter >> addBehaviorPanel [

	behaviorMessageLabel label: driver checkBehaviorPreconditions.
	
	(topLayout children includes: behaviorPanel)
		ifFalse: [ topLayout add: behaviorPanel expand: false ]
]

{ #category : 'private' }
StRenameMethodPreviewPresenter >> argumentPermutationChanged [
	driver setPermutation: permutation.
	self schedulePreviewUpdate
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> argumentsListHeight [
    | rowHeight |

    rowHeight := 25.

    ^ (arguments size max: 1) * rowHeight
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> behaviorMessageLabel [
	^ behaviorMessageLabel
]

{ #category : 'action' }
StRenameMethodPreviewPresenter >> browseOverrides [ 

	driver browseOverrides 
]

{ #category : 'private' }
StRenameMethodPreviewPresenter >> buildDiffFor: aChange [

	^ diffPresenter
		  leftText: aChange oldVersionTextToDisplay;
		  rightText: aChange textToDisplay
]

{ #category : 'private' }
StRenameMethodPreviewPresenter >> buildLeftSideDiff [

	diffPresenter 
		leftText: driver originalMessage contextUser source ;
		rightText: ''
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> buttonOk: aButton [

	buttonOk := aButton.
	self updateOkButtonState
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> canPermuteArguments [

	^ driver hasPrimitiveImplementor not
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> changesFrom: aCompositeChange [

	compositeChange := aCompositeChange.
	"we keep the composite because we want to use it for performing changes later on."
	
	changes := aCompositeChange changesToDisplayIn: self.
	self updateScopeList.
	self updateChanges.
	self updateTablePresenter
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> changesFrom: aCompositeChange scopes: aCollection [

	scopes := aCollection.
	self changesFrom: aCompositeChange.
]

{ #category : 'action' }
StRenameMethodPreviewPresenter >> closeWindow [

	self withWindowDo: [ :window | window close ]
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> connectPresenters [

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
	buttonBrowseMethod action: [ self browseOverrides ] .
	buttonUp action: [ self pushUpSelectedArgument ].
	buttonDown action: [ self pushDownSelectedArgument ].
	
	newNameInput whenTextChangedDo: [ self schedulePreviewUpdate ] .
]

{ #category : 'api - focus' }
StRenameMethodPreviewPresenter >> defaultKeyboardFocus [

	^ buttonOk
]

{ #category : 'layout' }
StRenameMethodPreviewPresenter >> defaultLayout [
	| scopesLayout nameLayout inputLayout argsLayout argumentLayout argButtonsLayout behaviorHeaderLayout |
	
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
	
	argsLayout := SpBoxLayout newTopToBottom 
		spacing: 2;
		add: argumentsList;
		yourself .
		
	argButtonsLayout := SpBoxLayout newTopToBottom 
		spacing: 2;
		add: buttonUp  expand: false;
		add: buttonDown expand: false;
		yourself .
		
	argumentLayout := SpBoxLayout newLeftToRight
   		spacing: 5;
    	add: argLabel expand: false;
    	add: argumentsList withConstraints: [ :constraints |
        	constraints
            height: self argumentsListHeight ];
    	add: argButtonsLayout expand: false;
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
		
	topLayout := SpBoxLayout newTopToBottom 
		spacing: 10; 
		add: nameLayout expand: false;
		add: argumentLayout expand: false;
		yourself.
	
	mainLayout := SpBoxLayout newTopToBottom
		spacing: 5;
		add: topLayout expand: false;
		add: scopesLayout expand: false;
		add: table;
		add: diffPresenter;
		yourself.
		
	^ mainLayout
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> driver: aDriver [

	driver := aDriver 
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> globalActiveScope [

	^ ScopesManager activeScope
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> hasWarning [

	^ driver notNil and: [ driver hasFailedBehaviorPreservingPreconditions ]
]

{ #category : 'action' }
StRenameMethodPreviewPresenter >> hideBehaviorPanel [
	
	(topLayout children includes: behaviorPanel)
		ifFalse: [ ^ self ].
		
	topLayout remove: behaviorPanel.
		self rebuildLayout 
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> initializeDialogWindow: aDialogWindowPresenter [

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
StRenameMethodPreviewPresenter >> initializePresenters [

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
		icon: (self iconNamed: #smallCancel).
		
	newNameLabel := self newLabel label: 'Selector'.
	newNameInput := self newTextInput.
	
	validationLabel := self newLabel label: ''.
	
	argLabel := self newLabel label: 'Arguments'.
	buttonUp := self newButton label: 'Up'; addStyle: 'small'.
	buttonDown := self newButton label: 'Dn'.
	nameIsValid := false. 
	
	behaviorWarningIcon := self newImage image: (self iconNamed: #warningIcon) .
	behaviorWarningTitle := self newLabel label: 'Behavior-preserving warnings' .
	behaviorMessageLabel := self newLabel label: ''.
	behaviorPanel := SpPresenter new .
	
	buttonBrowseMethod := self newButton
		label: 'Browse affected methods';
		icon: (self iconNamed: #search).
		
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> isValidNameChange [
	^ driver refactoring areArgumentsPermuted
		or: [ driver newName ~= driver refactoring oldSelector ]
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> newNameInput [

	^ newNameInput
]

{ #category : 'action' }
StRenameMethodPreviewPresenter >> performChanges [
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
StRenameMethodPreviewPresenter >> pushDownSelectedArgument [
	| selectedIndex |

	self canPermuteArguments ifFalse: [ ^ self ].

	selectedIndex := argumentsList selection selectedIndex.

	selectedIndex >= argumentsList items size ifTrue: [ ^ self ].

	argumentsList items
		swap: selectedIndex
		with: selectedIndex + 1.

	permutation
		swap: selectedIndex
		with: selectedIndex + 1.

	argumentsList selectIndex: selectedIndex + 1.
	self argumentPermutationChanged
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> pushUpSelectedArgument [
    | selectedIndex |

	 self canPermuteArguments ifFalse: [ ^ self ].

    selectedIndex := argumentsList selection selectedIndex.

    selectedIndex <= 1 ifTrue: [ ^ self ].

    argumentsList items
        swap: selectedIndex
        with: selectedIndex - 1.

	 permutation
		swap: selectedIndex
		with: selectedIndex - 1.

    argumentsList selectIndex: selectedIndex - 1.
	 self argumentPermutationChanged
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> refactoring: aRBRemoveMethodDriver [ 
	refactoring := aRBRemoveMethodDriver
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> showValidationError: message [

	nameIsValid := false.
	validationLabel label:
		(message asText
			addAttribute: (TextColor color: Smalltalk ui theme dangerTextColor);
			yourself)
]

{ #category : 'update' }
StRenameMethodPreviewPresenter >> updateActiveScope [

	activeScope := scopeDropList selectedItem asRBEnvironment
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> updateArgumentButtons [

	| selectedIndex |

	self canPermuteArguments ifFalse: [
		buttonUp disable.
		buttonDown disable.
		^ self ].

	selectedIndex := argumentsList selection selectedIndex.

	selectedIndex <= 1
		ifTrue: [ buttonUp disable ]
		ifFalse: [ buttonUp enable ].

	selectedIndex >= argumentsList items size
		ifTrue: [ buttonDown disable ]
		ifFalse: [ buttonDown enable ].

	^ self
]

{ #category : 'update' }
StRenameMethodPreviewPresenter >> updateChanges [

	changes ifNil: [ changesInScope := OrderedCollection new.
		selectedChanges := OrderedCollection new ]
	
	ifNotNil: [ 
		changesInScope := changes select: [ :change |
							change isInScope: activeScope ].
		selectedChanges := OrderedCollection withAll: changesInScope ]

	
]

{ #category : 'update' }
StRenameMethodPreviewPresenter >> updateGlobalActiveScope [

	ScopesManager activeScope: scopeDropList selectedItem asRBEnvironment
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> updateOkButtonState [

	nameIsValid
		ifTrue: [ buttonOk enable ]
		ifFalse: [ buttonOk disable ]
]

{ #category : 'update' }
StRenameMethodPreviewPresenter >> updateScopeList [

	scopeDropList items: scopes.
	self globalActiveScope 
		ifNotNil: [ :scope | scopeDropList selectItem: scope ]
		ifNil: [ scopeDropList selectFirst ]
]

{ #category : 'update' }
StRenameMethodPreviewPresenter >> updateTablePresenter [

	table items: changesInScope.
	table items ifNotEmpty: [ table selectIndex: 1 ]
]

{ #category : 'initialization' }
StRenameMethodPreviewPresenter >> validationLabel [
	^ validationLabel 
]

{ #category : 'accessing' }
StRenameMethodPreviewPresenter >> windowTitle [ 
	
	^ 'Refactoring changes'
]
