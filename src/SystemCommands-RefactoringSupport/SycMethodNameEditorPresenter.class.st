"
I am an UI used to rename methods.

HOW TO RUN

SycMethodNameEditor
		openOn:
			(RBMethodName
				selector: (UseOnlyForTest >> #bug:) selector
				arguments: ((UseOnlyForTest >> #bug:) ast arguments collect: #name))
				

My jobs is to:
	- display a preview of your future method name or '(invalid)' when the name is incorrect  (with a LabelPresenter previewResult an instance variable)
	- display the arguments list of your method and let you modified the order 

Public API and Key Messages

See class side 'example' script to know how to use me.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	selectorInput: 		<TextInputFieldPresenter>	An input field containing the new name of the method.
	argumentsList: 		<ListPresenter>					A list containing the arguments of the method.
	previewResult: 		<LabelPresenter>				A label to preview the new name of the method with the arguments.
	upButton: 			<ButtonPresenter>				A button update the order of the arguments.
	downButton: 			<ButtonPresenter>  			A button update the order of the arguments.
	methodName 		 	<RBMethod>						A RBMethodName to ensure the validity of the new name.
"
Class {
	#name : #SycMethodNameEditorPresenter,
	#superclass : #SpPresenter,
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
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #examples }
SycMethodNameEditorPresenter class >> example2 [
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

{ #category : #specs }
SycMethodNameEditorPresenter class >> openOn: aMethod [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	^ temp openBlockedDialog
]

{ #category : #specs }
SycMethodNameEditorPresenter class >> openOn: aMethod canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	^ temp openBlockedDialog
]

{ #category : #specs }
SycMethodNameEditorPresenter class >> openOn: aMethod withInvalidArgs: aSet canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	temp invalidArgNames: aSet.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	^ temp openBlockedDialog
]

{ #category : #specs }
SycMethodNameEditorPresenter class >> openOn: aMethod withInvalidArgs: aSet canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 canEditName: aBoolean4 [

	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."

	| temp |
	temp := self on: aMethod.
	temp invalidArgNames: aSet.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	temp canEditName: aBoolean4.
	^ temp openBlockedDialog
]

{ #category : #action }
SycMethodNameEditorPresenter >> addArgument [

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

{ #category : #action }
SycMethodNameEditorPresenter >> addArgumentAfter: anItem [

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

{ #category : #accessing }
SycMethodNameEditorPresenter >> addButton [
	^ addButton
]

{ #category : #services }
SycMethodNameEditorPresenter >> alert: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil
		ifFalse: [ self uiManager alert: aString ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> argumentsList [
	^ argumentsList
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> argumentsListMenu [
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

{ #category : #accessing }
SycMethodNameEditorPresenter >> canAddArgs [
	^canAddArgs ifNil: [ canAddArgs := false ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> canAddArgs: aBoolean [
	canAddArgs := aBoolean.
	addButton enabled: canAddArgs
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> canEditName [
	^canEditName  ifNil: [ canEditName := true ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> canEditName: aBoolean [
	canEditName := aBoolean.
	selectorInput enabled: canEditName
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> canRemoveArgs: anObject [

	argumentsList items do: [ :arg | arg canBeRemoved: anObject ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> canRenameArgs: anObject [

	argumentsList items do: [ :arg | arg canBeRenamed: anObject ]
]

{ #category : #action }
SycMethodNameEditorPresenter >> computePermutation [
	| index |
	index := 0.
	^ argumentsList items collect: [ :e |
		args indexOf: e name ifAbsent: [ index := index -1. index ]
	 ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> connectPresenters [

	selectorInput
		bindKeyCombination: Character cr asKeyCombination
		toAction: [ :p | self renameMethodAndClose: self owner ];

		bindKeyCombination: Character escape asKeyCombination
			toAction: [ self owner triggerCancelAction; close ]
]

{ #category : #layout }
SycMethodNameEditorPresenter >> defaultLayout [
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

{ #category : #'accessing - ui' }
SycMethodNameEditorPresenter >> downButton [
	^ downButton
]

{ #category : #action }
SycMethodNameEditorPresenter >> getDefaultValue [

	^ self uiManager
		  request: 'Enter default value for argument'
		  initialAnswer: 'nil'
]

{ #category : #action }
SycMethodNameEditorPresenter >> getNewKeywordName [

	^ self uiManager
		  request: 'Enter name for new keyword'
		  initialAnswer: 'arg'
]

{ #category : #services }
SycMethodNameEditorPresenter >> getParametersOrder [
	^ argumentsList items collect: [ :arg | arg newName ]
]

{ #category : #initialization }
SycMethodNameEditorPresenter >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		addButton: 'Rename' do: [ :presenter | self renameMethodAndClose: presenter ];
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		initialExtent: 600 @ 300 ;
		title: 'Method name editor', (methodName selector
					ifNil: [ '' ]
					ifNotNil: [ :selector | ' : "', selector, '"' ])
]

{ #category : #initialization }
SycMethodNameEditorPresenter >> initializePresenters [

	selectorInput := self newTextInput.
	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	selectorInput editable: self canEditName.
	selectorInput editable: self canEditName.

	previewResult := self newLabel.

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
	addButton := self newButton.
	addButton
		addStyle: 'small';
		label: '+';
		action: [
			argumentsList items
				ifEmpty: [ self addArgument ]
				ifNotEmpty: [ self addArgumentAfter: argumentsList items last ] ].

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

{ #category : #accessing }
SycMethodNameEditorPresenter >> invalidArgNames [
	^ invalidArgNames ifNil: [ invalidArgNames := { } ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> invalidArgNames: aSet [
	invalidArgNames := aSet
]

{ #category : #'instance creation' }
SycMethodNameEditorPresenter >> newArgName [
	| baseString index newString |
	newString := baseString := 'anObject'.
	index := 0.
	[ self invalidArgNames, (self newArgs collect: #name) includes: newString ]
		whileTrue:
			[index := index + 1.
			newString := baseString , index printString].
	^ newString
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> newArgs [
	^ argumentsList items select: [ :e | (args includes: e name) not ]
]

{ #category : #'accessing - ui' }
SycMethodNameEditorPresenter >> previewResult [
	^ previewResult
]

{ #category : #action }
SycMethodNameEditorPresenter >> pushDownSelectedArgument [
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

{ #category : #action }
SycMethodNameEditorPresenter >> pushUpSelectedArgument [
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

{ #category : #accessing }
SycMethodNameEditorPresenter >> removeArgument: anItem [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	argumentsList items: (argumentsList items copyUpTo: anItem), (argumentsList items copyAfter: anItem).
	argumentsList selectIndex: selectedIndex - 1.
	self updateLabel
]

{ #category : #action }
SycMethodNameEditorPresenter >> renameArgument: anItem [

	| argName selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	argName := self uiManager
		request: 'Enter default name of argument' initialAnswer: anItem.
	argName ifNil: [ CmdCommandAborted signal ].
	(self invalidArgNames includes: argName) ifTrue: [
		self inform: 'It''s a invalid name'.
		CmdCommandAborted signal ].
	anItem newName: argName.
	argumentsList items: (argumentsList items copyUpTo: anItem), {anItem}, (argumentsList items copyAfter: anItem).
	argumentsList selectIndex: selectedIndex.
	self updateLabel
]

{ #category : #services }
SycMethodNameEditorPresenter >> renameMap [
	^ argumentsList items select: [ :arg | arg hasNewName ]
]

{ #category : #action }
SycMethodNameEditorPresenter >> renameMethodAndClose: presenter [
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

{ #category : #'accessing - ui' }
SycMethodNameEditorPresenter >> selectorInput [
	^ selectorInput
]

{ #category : #'accessing - model' }
SycMethodNameEditorPresenter >> setModelBeforeInitialization: aRBMethodName [
	methodName := aRBMethodName.
	args := methodName arguments copy
]

{ #category : #'accessing - ui' }
SycMethodNameEditorPresenter >> upButton [
	^ upButton
]

{ #category : #action }
SycMethodNameEditorPresenter >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult
		label:
			(RBMethodName
				selector: (self selectorInput text ifNil: [''])
				arguments: (argumentsList items collect: #newName) ) methodName
]

{ #category : #initialization }
SycMethodNameEditorPresenter >> updatePresenter [

	selectorInput text: methodName selector.
	previewResult label: methodName methodName
]
