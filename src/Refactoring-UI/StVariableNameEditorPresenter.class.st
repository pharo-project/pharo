"
It provides a presenter for editing a variable name. The editor ensures the entered variable name is non-empty.

Additionally, it provides options to handle validation and user interaction, such as:
	• Entering a variable name in the provided input field.
	• Validating the variable name to ensure it is not empty.
	• Confirming the variable name input using the 'Rename' button or keyboard shortcut (Enter).
	• Cancelling the operation using the 'Cancel' button or the 'Escape'

## Example

To edit a variable name, use the following snippet:
```
StVariableNameEditorPresenter
	openOn: ('exampleName').
```

This will open the variable name editor with the specified name pre-filled in the input field.
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
		toAction: [ :p | self extractToVariableAndClose: self owner ];

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
			add: 'Variable name:'; add: variableInput; nextRow;
			add: errorLabel; nextRow ];
		yourself
]

{ #category : 'action' }
StVariableNameEditorPresenter >> extractToVariableAndClose: presenter [

	 | variable |
    variable := variableInput text.
    variable isEmptyOrNil ifTrue: [
    	self showError: 'Variable name cannot be empty. Please enter a variable name.'. ^ self ].
	variableName := variable.
	presenter 
		beOk; 
		close. 
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		initialExtent: 600 @ 300 ;
		title: 'Variable name editor';
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		addDefaultButton: 'Extract' do: [ :presenter | self extractToVariableAndClose: presenter ];
		whenOpenedDo: [ variableInput takeKeyboardFocus; selectAll ]
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> initializePresenters [
	variableInput := self newTextInput.
	errorLabel := self newLabel
]

{ #category : 'accessing - model' }
StVariableNameEditorPresenter >> setModelBeforeInitialization: aRBVariableName [
	variableName := aRBVariableName
]

{ #category : 'printing' }
StVariableNameEditorPresenter >> showError: aString [
	errorLabel label: aString
]

{ #category : 'initialization' }
StVariableNameEditorPresenter >> updatePresenter [
	variableInput text: variableName.
]

{ #category : 'accessing' }
StVariableNameEditorPresenter >> variableName [
	^ variableName 
]
