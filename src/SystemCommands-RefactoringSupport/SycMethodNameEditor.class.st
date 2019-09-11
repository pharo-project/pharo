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
	#name : #SycMethodNameEditor,
	#superclass : #SpPresenter,
	#instVars : [
		'selectorInput',
		'argumentsList',
		'previewResult',
		'upButton',
		'downButton',
		'methodName',
		'editParameter',
		'okEdit',
		'editerArgumentList',
		'refactoring',
		'isEditable'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
SycMethodNameEditor class >> defaultEditableSpec [
	<spec>
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: 'Selector'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #selectorInput;
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: 'Arguments'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #argumentsList;
				add:
					(SpBoxLayout newVertical
						add: #upButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						add: #downButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: 'Edit'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #editParameter; 
				add:
					(SpBoxLayout newVertical
						add: #okEdit
							withConstraints: [ :aConstraints | aConstraints height: 20 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: 'Preview'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #previewResult yourself);
		yourself
]

{ #category : #specs }
SycMethodNameEditor class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: 'Selector'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #selectorInput;
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: 'Arguments'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #argumentsList;
				add:
					(SpBoxLayout newVertical
						add: #upButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						add: #downButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: 'Preview'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #previewResult yourself);
		yourself
]

{ #category : #example }
SycMethodNameEditor class >> example2 [
	<script>
	self
		openOn:
			(RBMethodName
				selector: (RBExtractMethodRefactoring >> #validateRenameOf:to:) selector
				arguments: ((RBExtractMethodRefactoring >> #validateRenameOf:to:) ast arguments collect: #name))
]

{ #category : #specs }
SycMethodNameEditor class >> openOn: aMethod [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	|temp|
	temp := (self on: aMethod).
	temp isEditable: false.
	temp initializeEditerArgumentList.
	^ temp openModalWithSpec
]

{ #category : #specs }
SycMethodNameEditor class >> openOn: aMethod withRefactoring: refactoring [
	|temp|
	temp := (self on: aMethod).
	temp refactoring: refactoring.
	temp isEditable: true.
	temp initializeEditerArgumentList.
	^ temp openModalWithSpec
]

{ #category : #services }
SycMethodNameEditor >> alert: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil
		ifFalse: [ UIManager default alert: aString ]
]

{ #category : #accessing }
SycMethodNameEditor >> argumentsList [
	^ argumentsList
]

{ #category : #accessing }
SycMethodNameEditor >> downButton [
	^ downButton
]

{ #category : #services }
SycMethodNameEditor >> editParameter [
	| selected newName|
	newName := editParameter text asSymbol.
	selected := argumentsList items at: (argumentsList selection selectedIndex).
	newName = selected ifTrue: [ ^ self ].
	refactoring parameterMap: editerArgumentList;
		   		   validateRenameOf: selected to: newName.
	editerArgumentList at: (self getKeyOf: selected) put: newName.
	self updateItem: selected to: newName.
	self updateLabel
]

{ #category : #services }
SycMethodNameEditor >> getKeyOf: aValue [
	|keyOfValue|
	editerArgumentList keysAndValuesDo: [ :key :value | 
		aValue = value ifTrue: [ keyOfValue := key ] ].
	^ keyOfValue
]

{ #category : #services }
SycMethodNameEditor >> getParametersOrder [
	^ argumentsList items collect: [ :each | self getKeyOf: each ]
	
]

{ #category : #initialization }
SycMethodNameEditor >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		closeOnBackdropClick: true;
		addButton: 'Rename' do: [ :presenter | self renameMethodAndClose: presenter ];
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		initialExtent: 600 @ 300 ;
		title: 'Method name editor'
]

{ #category : #services }
SycMethodNameEditor >> initializeEditerArgumentList [
	editerArgumentList := Dictionary new.
	methodName arguments do: [ :each | 
		editerArgumentList at: each put: each ]
]

{ #category : #initialization }
SycMethodNameEditor >> initializePresenter [
	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	upButton action: [ self pushUpSelectedArgument ].
	downButton action: [ self pushDownSelectedArgument ].

	argumentsList
		whenModelChangedDo: [ :model | 
			argumentsList selectIndex: 1.
			model
				ifEmpty: [ upButton disable.
					downButton disable ] ].

	argumentsList items: methodName arguments.
	self updateEditTemp.
	
	argumentsList whenSelectionChangedDo: [ self updateEditTemp ].
	okEdit action: [ self validateEditParameter ]

]

{ #category : #initialization }
SycMethodNameEditor >> initializeWidgets [
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
	okEdit := self newButton.
	
	okEdit label: 'Ok'
	
]

{ #category : #accessing }
SycMethodNameEditor >> isEditable: aBoolean [ 
	isEditable := aBoolean .
	
	isEditable ifTrue: [ self buildWithSpecLayout: self class defaultEditableSpec ]
	ifFalse: [ self buildWithSpecLayout: self class defaultSpec ]
	
]

{ #category : #services }
SycMethodNameEditor >> okEdit [
	^ okEdit
]

{ #category : #accessing }
SycMethodNameEditor >> previewResult [
	^ previewResult
]

{ #category : #action }
SycMethodNameEditor >> pushDownSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	selectedIndex = argumentsList items size
		ifTrue: [ ^ self inform: 'The argument is already the last of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex + 1.
	argumentsList selectIndex: selectedIndex + 1.
	self updateLabel
]

{ #category : #action }
SycMethodNameEditor >> pushUpSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	selectedIndex = 1
		ifTrue: [ ^ self inform: 'The argument is already the first of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex - 1.
	argumentsList selectIndex: selectedIndex - 1.
	self updateLabel
]

{ #category : #services }
SycMethodNameEditor >> refactoring [
	^ refactoring
]

{ #category : #services }
SycMethodNameEditor >> refactoring: anObject [
	refactoring := anObject 
]

{ #category : #action }
SycMethodNameEditor >> renameMethodAndClose: presenter [
	^ self previewResult label = '(invalid)'
		ifTrue: [ self inform: 'Invalid method name' ]
		ifFalse: [
			refactoring ifNotNil: [ refactoring parameterMap: editerArgumentList].
			methodName
				arguments: self getParametersOrder;
				selector: selectorInput text.
			presenter
				beOk;
				close ]
]

{ #category : #accessing }
SycMethodNameEditor >> selectorInput [
	^ selectorInput
]

{ #category : #'accessing model' }
SycMethodNameEditor >> setModelBeforeInitialization: aRBMethodName [
	methodName := aRBMethodName
]

{ #category : #accessing }
SycMethodNameEditor >> upButton [
	^ upButton
]

{ #category : #services }
SycMethodNameEditor >> updateEditTemp [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	selectedIndex = 0 ifFalse: [ 
	editParameter text: (argumentsList items at: selectedIndex)]
]

{ #category : #services }
SycMethodNameEditor >> updateItem: selected to: newName [
	argumentsList items doWithIndex: [ :each :index | 
		each = selected ifTrue: [ argumentsList items at: index put: newName  ] ]
]

{ #category : #action }
SycMethodNameEditor >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult
		label:
			(RBMethodName
				selector: self selectorInput text
				arguments: argumentsList items) methodName
]

{ #category : #services }
SycMethodNameEditor >> validateEditParameter [
	self previewResult label = '(invalid)'
		ifTrue: [ self alert: 'You can not rename parameters if you have an invalid method name' ]
		ifFalse: [ [ self editParameter  ]
 			on: RBRefactoringError 
			do: [:ex | self alert: ex messageText] 
	]
]
