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
		'methodName'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

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

{ #category : #specs }
SycMethodNameEditor class >> openOn: aMethod [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."
	^ (self on: aMethod) openModalWithSpec
]

{ #category : #accessing }
SycMethodNameEditor >> argumentsList [
	^ argumentsList
]

{ #category : #accessing }
SycMethodNameEditor >> downButton [
	^ downButton
]

{ #category : #initialization }
SycMethodNameEditor >> initializeDialogWindow: aModalPresenter [
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

	argumentsList items: methodName arguments
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
	previewResult label: methodName methodName
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

{ #category : #action }
SycMethodNameEditor >> renameMethodAndClose: presenter [
	^ self previewResult label = '(invalid)'
		ifTrue: [ self inform: 'Invalid method name' ]
		ifFalse: [ methodName
				arguments: argumentsList items;
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

{ #category : #action }
SycMethodNameEditor >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult
		label:
			(RBMethodName
				selector: self selectorInput text
				arguments: self argumentsList items) methodName
]
