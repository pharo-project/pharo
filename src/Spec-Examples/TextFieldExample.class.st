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
	#category : #Spec-Examples
}

{ #category : #specs }
TextFieldExample class >> defaultSpec [
	| textFieldHeight |
	
	textFieldHeight := StandardFonts defaultFont height + 15.
	^ SpecLayout composed
	 	add: #textField origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@textFieldHeight;
	 	add: #methodBrowser origin: 0@0 corner: 1@1 offsetOrigin: 0@textFieldHeight offsetCorner: 0@textFieldHeight;
		yourself 
]

{ #category : #example }
TextFieldExample class >> example [
	^ self new openWithSpec
]

{ #category : #specs }
TextFieldExample class >> title [

	^ 'Text Field Example'
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
		ghostText: 'Enter a class name';
		autoAccept: true;
		removeEntryCompletion.
	methodBrowser 
		sortingBlock: [:a :b | a selector < b selector ];
		displayBlock: [:m | { m selector } ]
]

{ #category : #accessing }
TextFieldExample >> methodBrowser [

	^ methodBrowser
]

{ #category : #accessing }
TextFieldExample >> textField [

	^ textField
]
