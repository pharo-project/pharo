"
It provides a presenter for editing a method name. The editor also handles arguments and currently can:

- Add an argument by clicking in `+` button. 
- Remove an argument (currently selecting the argument, right-click to bring a contextual menu and then choose `Remove` option).
- Re-order arguments by clicking on each argument and using up and down arrows.

## Example

To edit a method with one argument:
```
StMethodNameEditorPresenter
	openOn: (RBMethodName
		selector: (ObjectMockForTest >> #variable:) selector
		arguments: #(#arg1))
		canRenameArgs: true
		canRemoveArgs: true
		canAddArgs: true.
```

To edit a method without arguments:
```
StMethodNameEditorPresenter
	openOn: (RBMethodName
		selector: (ObjectMockForTest >> #variable) selector
		arguments: #())
		canRenameArgs: true
		canRemoveArgs: true
		canAddArgs: true.
```
"
Class {
	#name : 'StMethodNameEditorPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'selectorInput',
		'argumentsList',
		'previewResult',
		'upButton',
		'downButton',
		'addButton',
		'methodName',
		'args',
		'invalidArgNames',
		'canAddArgs',
		'canEditName'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'examples' }
StMethodNameEditorPresenter class >> example2 [
	<script>
	self
		openOn:
			(RBMethodName
				selector: (RBExtractMethodRefactoring >> #validateRenameOf:to:) selector
				arguments: ((RBExtractMethodRefactoring >> #validateRenameOf:to:) ast arguments collect: [:each | each name]))
		canRenameArgs: true
		canRemoveArgs: true
		canAddArgs: true
]

{ #category : 'specs' }
StMethodNameEditorPresenter class >> openOn: aMethod [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	^ temp openBlockedDialog
]

{ #category : 'specs' }
StMethodNameEditorPresenter class >> openOn: aMethod canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	^ temp openBlockedDialog
]

{ #category : 'specs' }
StMethodNameEditorPresenter class >> openOn: aMethod withInvalidArgs: aSet canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	temp invalidArgNames: aSet.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	^ temp openBlockedDialog
]

{ #category : 'specs' }
StMethodNameEditorPresenter class >> openOn: aMethod withInvalidArgs: aSet canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 canEditName: aBoolean4 [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."

	| temp |
	temp := self on: aMethod.
	temp invalidArgNames: aSet.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	temp canEditName: aBoolean4.
	^ temp openModal
]

{ #category : 'action' }
StMethodNameEditorPresenter >> addArgument [

	| newArg argValue newKeyword |
	newKeyword := self getNewKeywordName.
	newKeyword isEmptyOrNil ifTrue: [ CmdCommandAborted signal ].
	newArg := self newArgName asSymbol.
	argValue := self getDefaultValue.
	argValue isEmptyOrNil ifTrue: [ CmdCommandAborted signal ].
	newArg := RBArgumentName name: newArg value: argValue.
	argumentsList items: { newArg }.
	self selectorInput text: self selectorInput text , newKeyword ,':' .
	argumentsList selectIndex: 1.
	self canEditName: true.
	self updateLabel
]

{ #category : 'action' }
StMethodNameEditorPresenter >> addArgumentAfter: anItem [

	| newArg argValue selectedIndex newKeyword |
	newKeyword := self getNewKeywordName.
	newKeyword isEmptyOrNil ifTrue: [ CmdCommandAborted signal ].
	selectedIndex := argumentsList selection selectedIndex.
	newArg := self newArgName asSymbol.
	argValue := self getDefaultValue.
	argValue isEmptyOrNil ifTrue: [ CmdCommandAborted signal ].
	newArg := RBArgumentName name: newArg value: argValue.
	argumentsList items:
		(argumentsList items copyUpThrough: anItem) , { newArg }
		, (argumentsList items copyAfter: anItem).
	self selectorInput text: self selectorInput text , newKeyword, ':'.
	argumentsList selectIndex: selectedIndex + 1.
	self canEditName: true.
	self updateLabel
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> addButton [
	^ addButton
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> argumentsList [
	^ argumentsList
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> argumentsListMenu [
	^ self newMenu
		addGroup: [ :aGroup |
			aGroup
				addItem: [ :anItem |
					anItem
						name: 'Rename';
						visibleIf: [ argumentsList selectedItem canBeRenamed ];
						iconName: #edit;
						action: [ self renameArgument: argumentsList selectedItem ] ];
				addItem: [ :anItem |
					anItem
						name: 'Add';
						visibleIf: [ self canAddArgs ];
						iconName: #add;
						action: [ self addArgumentAfter: argumentsList selectedItem ] ];
				addItem: [ :anItem |
					anItem
						name: 'Remove';
						visibleIf: [ argumentsList selectedItem canBeRemoved ];
						shortcut: $x meta;
						iconName: #remove;
						description: 'Remove argument';
						action: [ self removeArgument: argumentsList selectedItem ] ] ]
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> canAddArgs [
	^ canAddArgs ifNil: [ canAddArgs := false ]
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> canAddArgs: aBoolean [
	canAddArgs := aBoolean.
	addButton enabled: canAddArgs
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> canEditName [
	^ canEditName ifNil: [ canEditName := true ]
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> canEditName: aBoolean [
	canEditName := aBoolean.
	selectorInput enabled: canEditName
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> canRemoveArgs: anObject [

	argumentsList items do: [ :arg | arg canBeRemoved: anObject ]
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> canRenameArgs: anObject [

	argumentsList items do: [ :arg | arg canBeRenamed: anObject ]
]

{ #category : 'action' }
StMethodNameEditorPresenter >> computePermutation [

	| index |
	index := 0.
	^ argumentsList items collect: [ :e |
		args indexOf: e name ifAbsent: [ index := index -1. index ] ]
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> connectPresenters [

	selectorInput
		bindKeyCombination: Character cr asKeyCombination
		toAction: [ :p | self renameMethodAndClose: self owner ];

		bindKeyCombination: Character escape asKeyCombination
			toAction: [ self owner triggerCancelAction; close ]
]

{ #category : 'layout' }
StMethodNameEditorPresenter >> defaultLayout [

	| argumentsEditor |

	argumentsEditor := SpBoxLayout newLeftToRight
		add: argumentsList;
		add: (SpBoxLayout newTopToBottom
				add: upButton expand: false;
				add: downButton expand: false;
				add: addButton expand: false;
				yourself)
			expand: false;
		yourself.

	^ SpGridLayout new
		borderWidth: 5;
		beColumnNotHomogeneous;
		column: 2 expand: true;
		build: [ :builder | builder
			add: 'Selector'; add: selectorInput; nextRow;
			add: 'Arguments'; add: argumentsEditor; nextRow;
			add: 'Preview'; add: previewResult; nextRow ];
		yourself
]

{ #category : 'accessing - ui' }
StMethodNameEditorPresenter >> downButton [
	^ downButton
]

{ #category : 'action' }
StMethodNameEditorPresenter >> getDefaultValue [

	^ self 
		  request: 'Enter default value for argument'
		  initialAnswer: 'nil'
]

{ #category : 'action' }
StMethodNameEditorPresenter >> getNewKeywordName [

	^ self
		  request: 'Enter name for new keyword'
		  initialAnswer: 'arg'
]

{ #category : 'services' }
StMethodNameEditorPresenter >> getParametersOrder [
	^ argumentsList items collect: [ :arg | arg newName ]
]

{ #category : 'to be pushed to presenter' }
StMethodNameEditorPresenter >> inform: aString [

	^ self application newInform
		title: aString;
		acceptLabel: 'Got it !';
		openDialog
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> initializeAddButton [

	addButton := self newButton.
	addButton
		addStyle: 'small';
		label: '+';
		action: [
			argumentsList items
				ifEmpty: [ self addArgument ]
				ifNotEmpty: [ self addArgumentAfter: argumentsList items last ].
			self updateReorderButtonStatus. ].
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> initializeArgumentsList [

	argumentsList := self newList.
	argumentsList addStyle: 'rows8'.
	argumentsList whenModelChangedDo: [ :model |
		model ifEmpty: [
			upButton disable.
			downButton disable ] ].
	argumentsList items: (methodName arguments collect: [:arg | RBArgumentName name: arg]);
		contextMenu: self argumentsListMenu.
	argumentsList items ifNotEmpty: [ argumentsList selectIndex: 1 ]
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> initializeButtons [

	upButton := self newButton.
	upButton
		addStyle: 'small';
		label: 'Up';
		action: [ self pushUpSelectedArgument ].
	downButton := self newButton.
	downButton
		addStyle: 'small';
		label: 'Dn';
		action: [ self pushDownSelectedArgument ].

	args size < 2 ifFalse: [ ^ self ].
	upButton disable.
	downButton disable
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		initialExtent: 600 @ 300 ;
		title: 'Method name editor', (methodName selector
					ifNil: [ '' ]
					ifNotNil: [ :selector | ' : "', selector, '"' ]);		
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		addDefaultButton: 'Rename' do: [ :presenter | self renameMethodAndClose: presenter ];
		whenOpenedDo: [ selectorInput takeKeyboardFocus; selectAll ]
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> initializePresenters [

	selectorInput := self newTextInput.
	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	selectorInput editable: self canEditName.
	selectorInput editable: self canEditName.

	previewResult := self newLabel.

	self 
		initializeButtons;
		initializeArgumentsList;
		initializeAddButton.
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> invalidArgNames [
	^ invalidArgNames ifNil: [ invalidArgNames := { } ]
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> invalidArgNames: aSet [
	invalidArgNames := aSet
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> methodName [

	^ methodName
]

{ #category : 'utilities' }
StMethodNameEditorPresenter >> newArgName [
	| baseString index newString |
	newString := baseString := 'anObject'.
	index := 0.
	[ self invalidArgNames, (self newArgs collect: [:each | each name]) includes: newString ]
		whileTrue:
			[index := index + 1.
			newString := baseString , index printString].
	^ newString
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> newArgs [
	^ argumentsList items select: [ :e | (args includes: e name) not ]
]

{ #category : 'accessing - ui' }
StMethodNameEditorPresenter >> previewResult [
	^ previewResult
]

{ #category : 'action' }
StMethodNameEditorPresenter >> pushDownSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	selectedIndex = 0
		ifTrue: [ ^ self inform: 'There is not argument selected.' ].
	selectedIndex = argumentsList items size
		ifTrue: [ ^ self inform: 'The argument is already the last of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex + 1.
	argumentsList selectIndex: selectedIndex + 1.
	self updateLabel
]

{ #category : 'action' }
StMethodNameEditorPresenter >> pushUpSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	selectedIndex = 0
		ifTrue: [ ^ self inform: 'There is not argument selected.' ].
	selectedIndex = 1
		ifTrue: [ ^ self inform: 'The argument is already the first of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex - 1.
	argumentsList selectIndex: selectedIndex - 1.
	self updateLabel
]

{ #category : 'accessing' }
StMethodNameEditorPresenter >> removeArgument: anItem [

	| selectedIndex |

	selectedIndex := argumentsList selection selectedIndex.
	argumentsList items: (argumentsList items copyUpTo: anItem), (argumentsList items copyAfter: anItem).
	argumentsList selectIndex: selectedIndex - 1.
	self updateLabel.
	self updateReorderButtonStatus.

]

{ #category : 'action' }
StMethodNameEditorPresenter >> renameArgument: anItem [

	| argName selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	argName := self request: 'Enter default name of argument' initialAnswer: anItem.
	argName ifNil: [ CmdCommandAborted signal ].
	(self invalidArgNames includes: argName) ifTrue: [
		self inform: 'It''s a invalid name'.
		CmdCommandAborted signal ].
	anItem newName: argName.
	argumentsList items: (argumentsList items copyUpTo: anItem), {anItem}, (argumentsList items copyAfter: anItem).
	argumentsList selectIndex: selectedIndex.
	self updateLabel
]

{ #category : 'services' }
StMethodNameEditorPresenter >> renameMap [

	^ argumentsList items select: [ :arg | arg hasNewName ]
]

{ #category : 'action' }
StMethodNameEditorPresenter >> renameMethodAndClose: presenter [

	^ self previewResult label = '(invalid)'
		ifTrue: [ self inform: 'Invalid method name' ]
		ifFalse: [
			methodName
				arguments: self getParametersOrder;
				selector: selectorInput text;
				newArgs: self newArgs;
				renameMap: self renameMap;
				permutation: self computePermutation.
			presenter
				beOk;
				close ]
]

{ #category : 'action' }
StMethodNameEditorPresenter >> request: aString initialAnswer: aSecondString [

	^ self application newRequest
			title: 'Information needed';
			label: aString;
			text: aSecondString;
			openModal
]

{ #category : 'accessing - ui' }
StMethodNameEditorPresenter >> selectorInput [
	^ selectorInput
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> setModelBeforeInitialization: aRBMethodName [
	methodName := aRBMethodName.
	args := methodName arguments copy
]

{ #category : 'accessing - ui' }
StMethodNameEditorPresenter >> upButton [
	^ upButton
]

{ #category : 'action' }
StMethodNameEditorPresenter >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult
		label:
			(RBMethodName
				selector: (self selectorInput text ifNil: [''])
				arguments: (argumentsList items collect: #newName) ) methodName
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> updatePresenter [

	selectorInput text: methodName selector.
	previewResult label: methodName methodName
]

{ #category : 'initialization' }
StMethodNameEditorPresenter >> updateReorderButtonStatus [
	"Method name editor dialog up/down buttons should be disabled with < 2 arguments"

	argumentsList items size < 2 
		ifTrue: [
			upButton disable.
			downButton disable ]
		ifFalse: [  
			upButton enable.
			downButton enable ]

]
