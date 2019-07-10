"
A TextFieldExample is a simple example of how to use TextFields.
"
Class {
	#name : #TextFieldExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'methodBrowser',
		'textField'
	],
	#category : #'Spec-Examples-Standalone'
}

{ #category : #specs }
TextFieldExample class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add: #textField withConstraints: [ :constraints | constraints height: StandardFonts defaultFont height + 15 ];
		add: #methodBrowser;
		yourself
]

{ #category : #example }
TextFieldExample class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #specs }
TextFieldExample class >> title [

	^ 'Text Field Example'
]

{ #category : #api }
TextFieldExample >> initialExtent [
	^ 900@550
]

{ #category : #initialization }
TextFieldExample >> initializePresenter [
	
	textField acceptBlock: [:text | Smalltalk globals at: text asSymbol
								ifPresent: [:class | methodBrowser messages: class methods ]
								ifAbsent: [ methodBrowser messages: #() ]]
]

{ #category : #initialization }
TextFieldExample >> initializeWidgets [
	methodBrowser := self instantiate: MessageBrowser.

	textField := self newTextInput.
	textField
		placeholder: 'Enter a class name';
		autoAccept: true;
		removeEntryCompletion.

	methodBrowser sortingBlock: [ :a :b | a selector < b selector ]
]

{ #category : #accessing }
TextFieldExample >> methodBrowser [

	^ methodBrowser
]

{ #category : #accessing }
TextFieldExample >> textField [

	^ textField
]
