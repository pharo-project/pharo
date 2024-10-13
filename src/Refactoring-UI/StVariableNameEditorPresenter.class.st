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
	#name : 'StVariableNameEditorPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'variableInput',
		'variableName',
		'errorLabel'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'specs' }
StVariableNameEditorPresenter class >> openOn: aVariable [

	|temp|
	temp := self on: aVariable.
	^ temp openBlockedDialog
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> connectPresenters [

	variableInput
		bindKeyCombination: Character cr asKeyCombination
		toAction: [ :p | self renameVariableAndClose: self owner ];

		bindKeyCombination: Character escape asKeyCombination
			toAction: [ self owner triggerCancelAction; close ]
]

{ #category : 'layout' }
StVariableNameEditorPresenter >> defaultLayout [

	^ SpGridLayout new
		borderWidth: 5;
		beColumnNotHomogeneous;
		column: 2 expand: true;
		build: [ :builder | builder
			add: 'Variable name'; add: variableInput; nextRow ];
		yourself
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		initialExtent: 400 @ 200 ;
		title: 'Variable name editor';	
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		addDefaultButton: 'Rename' do: [ :presenter | self renameVariableAndClose: presenter ];
		whenOpenedDo: [ variableInput takeKeyboardFocus; selectAll ]
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> initializePresenters [

	variableInput := self newTextInput
]

{ #category : 'action' }
StVariableNameEditorPresenter >> renameVariableAndClose: presenter [

	 | variable |
    variable := variableInput text.
    variable isEmptyOrNil ifTrue: [
        self showError: 'Variable name cannot be empty'.
        ^ CmdCommandAborted signal ].

	^ [ variableName
			newVariableName: variableInput text.
		presenter 
			beOk; 
			close].
]

{ #category : 'action' }
StVariableNameEditorPresenter >> request: aString initialAnswer: aSecondString [

	^ self application newRequest
			title: 'Information needed';
			label: aString;
			text: aSecondString;
			openModal
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> updatePresenter [

	variableInput text: variableName newVariableName.
]

{ #category : 'accessing - ui' }
StVariableNameEditorPresenter >> variableInput [
	^ variableInput 
]

{ #category : 'accessing' }
StVariableNameEditorPresenter >> variableName [
	^ variableName 
]
