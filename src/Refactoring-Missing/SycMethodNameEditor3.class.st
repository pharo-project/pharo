Class {
	#name : #SycMethodNameEditor3,
	#superclass : #ComposablePresenter,
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
		'refactoring'
	],
	#category : #'Refactoring-Missing'
}

{ #category : #specs }
SycMethodNameEditor3 class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add:
			(SpecBoxLayout newHorizontal
				add: 'Selector'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #selectorInput;
				yourself);
		add:
			(SpecBoxLayout newHorizontal
				add: 'Arguments'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #argumentsList;
				add:
					(SpecBoxLayout newVertical
						add: #upButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						add: #downButton
							withConstraints: [ :aConstraints | aConstraints height: 30 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpecBoxLayout newHorizontal
				add: 'Edit'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #editParameter; 
				add:
					(SpecBoxLayout newVertical
						add: #okEdit
							withConstraints: [ :aConstraints | aConstraints height: 20 ];
						yourself)
					withConstraints: [ :aConstraints | aConstraints width: 30 ];
				yourself);
		add:
			(SpecBoxLayout newHorizontal
				add: 'Preview'
					withConstraints: [ :aConstraints | aConstraints width: 80 ];
				add: #previewResult; yourself);
		yourself
]

{ #category : #specs }
SycMethodNameEditor3 class >> example [
	<script>
	self
		openOn:
			(RBMethodName
				selector: (UseOnlyForTest >> #a:b:) selector
				arguments: ((UseOnlyForTest >> #a:b:) ast arguments collect: #name))
]

{ #category : #specs }
SycMethodNameEditor3 class >> example2 [
	<script>
	self
		openOn:
			(RBMethodName
				selector: (UseOnlyForTest >> #a:b:) selector
				arguments: ((UseOnlyForTest >> #a:b:) ast arguments collect: #name))
]

{ #category : #specs }
SycMethodNameEditor3 class >> openOn: aMethod [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."

	^ (self on: aMethod) openModalWithSpec
]

{ #category : #specs }
SycMethodNameEditor3 class >> openOn: aMethod withRefactoring: refactoring [
	|temp|
	temp := (self on: aMethod).
	temp refactoring: refactoring.
	temp initializeEditerArgumentList.
	^ temp openModalWithSpec
]

{ #category : #services }
SycMethodNameEditor3 >> alert: aString [
	"Display a message for the user to read and then dismiss."

	aString isEmptyOrNil
		ifFalse: [ UIManager default alert: aString ]
]

{ #category : #accessing }
SycMethodNameEditor3 >> argumentsList [
	^ argumentsList
]

{ #category : #accessing }
SycMethodNameEditor3 >> downButton [
	^ downButton
]

{ #category : #services }
SycMethodNameEditor3 >> editParameter [
	"Validate the change of name the parameters and update a argumentsList and label"
	| selected newName|
	newName := editParameter text asSymbol.
	selected := argumentsList items at: (argumentsList selection selectedIndex).
	newName = selected ifTrue: [ ^ self ].
	refactoring dictParameters: editerArgumentList;
		   		   validateRenameOf: selected to: newName.
	editerArgumentList at: (self getKeyOf: selected) put: newName.
	self updateItem: selected to: newName.
	self updateLabel
]

{ #category : #services }
SycMethodNameEditor3 >> getKeyOf: aValue [
	"Get a real parameter's name of extracted method given a new parameter's name"
	|keyOfValue|
	editerArgumentList keysAndValuesDo: [ :key :value | 
		aValue = value ifTrue: [ keyOfValue := key ] ].
	^ keyOfValue
]

{ #category : #services }
SycMethodNameEditor3 >> getParametersOrder [
	"Get the new parameters order with the old parameters names"
	^ argumentsList items collect: [ :each | self getKeyOf: each ]
	
]

{ #category : #accessing }
SycMethodNameEditor3 >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		closeOnBackdropClick: true;
		addButton: 'Rename' do: [ :presenter | self renameMethodAndClose: presenter ];
		addButton: 'Cancel' do: [ :presenter | 
			presenter
				beCancel;
				close ];
		initialExtent: 600 @ 300;
		title: 'Method name editor'
]

{ #category : #services }
SycMethodNameEditor3 >> initializeEditerArgumentList [
	"Create a dictionary with old parameters name as key and value.
	It value can change after if you rename a parameter"
	editerArgumentList := Dictionary new.
	methodName arguments do: [ :each | 
		editerArgumentList at: each put: each ]
]

{ #category : #services }
SycMethodNameEditor3 >> initializePresenter [ 
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

{ #category : #services }
SycMethodNameEditor3 >> initializeWidgets [
	selectorInput := self instantiate: TextInputFieldPresenter.
	argumentsList := self newList.
	previewResult := self newLabel.
	upButton := self newButton.
	downButton := self newButton.
	
	selectorInput autoAccept: true.
	upButton label: 'Up'.
	downButton label: 'Dn'.
	selectorInput text: methodName selector.
	previewResult label: methodName methodName.
	
	editParameter := self newTextInput autoAccept: true.
	okEdit := self newButton.
	
	okEdit label: 'Ok'
	
]

{ #category : #services }
SycMethodNameEditor3 >> okEdit [
	^ okEdit
]

{ #category : #accessing }
SycMethodNameEditor3 >> previewResult [
	^ previewResult
]

{ #category : #accessing }
SycMethodNameEditor3 >> pushDownSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selectedIndex.
	selectedIndex = argumentsList items size
		ifTrue: [ ^ self inform: 'The argument is already the last of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex + 1.
	argumentsList selectIndex: selectedIndex + 1.
	self updateLabel
]

{ #category : #accessing }
SycMethodNameEditor3 >> pushUpSelectedArgument [
	| selectedIndex |
	selectedIndex := argumentsList selectedIndex.
	selectedIndex = 1
		ifTrue: [ ^ self inform: 'The argument is already the first of the list.' ].
	argumentsList items swap: selectedIndex with: selectedIndex - 1.
	argumentsList selectIndex: selectedIndex - 1.
	self updateLabel
]

{ #category : #services }
SycMethodNameEditor3 >> refactoring: anObject [
	refactoring := anObject 
]

{ #category : #services }
SycMethodNameEditor3 >> renameMethodAndClose: presenter [
	^ self previewResult label = '(invalid)'
		ifTrue: [ self inform: 'Invalid method name' ]
		ifFalse: [
			refactoring dictParameters: editerArgumentList.
			methodName
				arguments: self getParametersOrder;
				selector: selectorInput text.
			presenter
				beOk;
				close ]
]

{ #category : #accessing }
SycMethodNameEditor3 >> selectorInput [
	^ selectorInput
]

{ #category : #accessing }
SycMethodNameEditor3 >> setModelBeforeInitialization: aRBMethodName [
	methodName := aRBMethodName
]

{ #category : #accessing }
SycMethodNameEditor3 >> upButton [
	^ upButton
]

{ #category : #services }
SycMethodNameEditor3 >> updateEditTemp [
	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	selectedIndex = 0 ifFalse: [ 
	editParameter text: (argumentsList items at: selectedIndex)]
]

{ #category : #services }
SycMethodNameEditor3 >> updateItem: selected to: newName [
	argumentsList items doWithIndex: [ :each :index | 
		each = selected ifTrue: [ argumentsList items at: index put: newName  ] ]
]

{ #category : #accessing }
SycMethodNameEditor3 >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult
		label:
			(RBMethodName
				selector: self selectorInput text
				arguments: self argumentsList items) methodName
]

{ #category : #services }
SycMethodNameEditor3 >> validateEditParameter [
	"Validate the rename of a parameter, if exist an error show an alert"
	self previewResult label = '(invalid)'
		ifTrue: [ self alert: 'You can not rename parameters if you have an invalid method name' ]
		ifFalse: [ [ self editParameter  ]
 			on: RBRefactoringError 
			do: [:ex | self alert: ex messageText] 
	]
]
