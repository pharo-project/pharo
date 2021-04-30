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
		'methodName',
		'editParameter',
		'refactoring',
		'newArgs',
		'args',
		'renameMap',
		'invalidArgNames',
		'canAddArgs'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
SycMethodNameEditorPresenter class >> defaultEditableSpec [
	<spec>
	^ SpBoxLayout newTopToBottom
		add:
			(SpBoxLayout newLeftToRight
				add: 'Selector'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #selectorInput;
				yourself);
		add:
			(SpBoxLayout newLeftToRight
				add: 'Arguments'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #argumentsList;
				add:
					(SpBoxLayout newTopToBottom
						add: #upButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						add: #downButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpBoxLayout newLeftToRight
				add: 'Preview'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #previewResult yourself);
		yourself
]

{ #category : #specs }
SycMethodNameEditorPresenter class >> defaultSpec [
	^ SpBoxLayout newTopToBottom
		add:
			(SpBoxLayout newLeftToRight
				add: 'Selector'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #selectorInput;
				yourself);
		add:
			(SpBoxLayout newLeftToRight
				add: 'Arguments'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #argumentsList;
				add:
					(SpBoxLayout newTopToBottom
						add: #upButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						add: #downButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpBoxLayout newLeftToRight
				add: 'Preview'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #previewResult yourself);
		yourself
]

{ #category : #example }
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
	^ temp openModalWithSpec
]

{ #category : #specs }
SycMethodNameEditorPresenter class >> openOn: aMethod canRenameArgs: aBoolean1 canRemoveArgs: aBoolean2 canAddArgs: aBoolean3 [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := self on: aMethod.
	temp canRenameArgs: aBoolean1.
	temp canRemoveArgs: aBoolean2.
	temp canAddArgs: aBoolean3.
	^ temp openModalWithSpec
]

{ #category : #specs }
SycMethodNameEditorPresenter class >> openOn: aMethod withRefactoring: refactoring [
	|temp|
	temp := (self on: aMethod).
	temp refactoring: refactoring.
	temp initializeEditerArgumentList.
	^ temp openModalWithSpec
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> addArgumentAfter: anItem [
	| newArg argValue |
	newArg := self newArgName asSymbol.
	argValue := UIManager default
		request: 'Enter default value for argument' initialAnswer: 'nil'.
	self newArgs at: newArg put: argValue.
	newArg := RBArgumentName name: newArg value: argValue.
	argumentsList items: (argumentsList items copyUpThrough: anItem), {newArg}, (argumentsList items copyAfter: anItem).
	self updateLabel
]

{ #category : #services }
SycMethodNameEditorPresenter >> alert: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil
		ifFalse: [ UIManager default alert: aString ]
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
	canAddArgs := aBoolean
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
	^ argumentsList items collect: [ :e | | realVal |
		realVal := renameMap keyAtValue: e ifAbsent: [ e ].
		args indexOf: realVal ifAbsent: [ (newArgs indexOfKey: realVal ifAbsent: [ 0 ]) * -1 ]
	 ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> defaultParameterPlaceholderString [
	"the placeholder string to be used in parameter inputs"
	^ 'aParameterName'
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> downButton [
	^ downButton
]

{ #category : #services }
SycMethodNameEditorPresenter >> getKeyOf: aValue [
	^ self renameMap keyAtValue: aValue ifAbsent: [ aValue ]
]

{ #category : #services }
SycMethodNameEditorPresenter >> getParametersOrder [
	^ argumentsList items collect: [ :each | self getKeyOf: each ]
	
]

{ #category : #initialization }
SycMethodNameEditorPresenter >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		closeOnBackdropClick: true;
		addButton: 'Rename' do: [ :presenter | self renameMethodAndClose: presenter ];
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		initialExtent: 600 @ 300 ;
		title: 'Method name editor'
]

{ #category : #initialization }
SycMethodNameEditorPresenter >> initializePresenter [
	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	upButton action: [ self pushUpSelectedArgument ].
	downButton action: [ self pushDownSelectedArgument ].

	argumentsList
		whenModelChangedDo: [ :model | 
			model
				ifEmpty: [ upButton disable.
					downButton disable ] ].

	argumentsList items: (methodName arguments collect: [:arg | RBArgumentName name: arg]);
		contextMenu: self argumentsListMenu.

]

{ #category : #initialization }
SycMethodNameEditorPresenter >> initializeWidgets [
	selectorInput := self instantiate: SpTextInputFieldPresenter.
	argumentsList := self newList.
	previewResult := self newLabel.
	upButton := self newButton.
	downButton := self newButton.
	
	selectorInput autoAccept: true.
	upButton label: 'Up'.
	downButton label: 'Dn'.
	selectorInput text: methodName selector.
	previewResult label: methodName methodName.
	editParameter := (self instantiate: SpTextInputFieldPresenter) autoAccept: true.
	editParameter placeholder: self defaultParameterPlaceholderString.
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> invalidArgNames [
	^ invalidArgNames ifNil: [ invalidArgNames := { } ]
]

{ #category : #'instance creation' }
SycMethodNameEditorPresenter >> newArgName [
	| baseString index newString |
	newString := baseString := 'arg'.
	index := 0.
	[ self invalidArgNames includes: newString ] 
		whileTrue: 
			[index := index + 1.
			newString := baseString , index printString].
	^newString
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> newArgs [
	^ newArgs ifNil: [ newArgs := OrderedDictionary new ]
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> newArgs: aDict [
	newArgs := aDict.
	argumentsList items: argumentsList items, newArgs keys. self flag: #TODO
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> previewResult [
	^ previewResult
]

{ #category : #action }
SycMethodNameEditorPresenter >> pushDownSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
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
	selectedIndex = 1
		ifTrue: [ ^ self inform: 'The argument is already the first of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex - 1.
	argumentsList selectIndex: selectedIndex - 1.
	self updateLabel
]

{ #category : #services }
SycMethodNameEditorPresenter >> refactoring [
	^ refactoring
]

{ #category : #services }
SycMethodNameEditorPresenter >> refactoring: anObject [
	refactoring := anObject 
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> removeArgument: anItem [
	self newArgs removeKey: anItem ifAbsent: [  ].
	argumentsList items: (argumentsList items copyUpTo: anItem), (argumentsList items copyAfter: anItem).
	self updateLabel
]

{ #category : #action }
SycMethodNameEditorPresenter >> renameArgument: anItem [

	| argName |
	argName := UIManager default
		request: 'Enter default name of argument' initialAnswer: anItem.
	argName ifNil: [ CmdCommandAborted signal ].
	(self invalidArgNames includes: argName) ifTrue: [ 
		self inform: 'It''s a invalid name'.
		CmdCommandAborted signal ].
	self renameMap at: anItem put: argName.
	argumentsList items: (argumentsList items copyUpTo: anItem), {argName}, (argumentsList items copyAfter: anItem).
	self updateLabel.
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> renameMap [
	^renameMap ifNil: [ renameMap := Dictionary new ]
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

{ #category : #accessing }
SycMethodNameEditorPresenter >> selectorInput [
	^ selectorInput
]

{ #category : #'accessing model' }
SycMethodNameEditorPresenter >> setModelBeforeInitialization: aRBMethodName [
	methodName := aRBMethodName.
	args := methodName arguments copy
]

{ #category : #accessing }
SycMethodNameEditorPresenter >> upButton [
	^ upButton
]

{ #category : #services }
SycMethodNameEditorPresenter >> updateItem: selected to: newName [
	argumentsList items doWithIndex: [ :each :index | 
		each = selected ifTrue: [ argumentsList items at: index put: newName  ] ]
]

{ #category : #action }
SycMethodNameEditorPresenter >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult
		label:
			(RBMethodName
				selector: self selectorInput text
				arguments: argumentsList items) methodName
]
