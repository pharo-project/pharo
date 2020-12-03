"
A TextFieldExample is a simple example of how to use TextFields.
"
Class {
	#name : #SpTextFieldExample,
	#superclass : #SpPresenter,
	#instVars : [
		'methodBrowser',
		'textField'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpTextFieldExample class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #textField withConstraints: [ :constraints | constraints height: StandardFonts defaultFont height + 15 ];
		add: #methodBrowser;
		yourself
]

{ #category : #example }
SpTextFieldExample class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #specs }
SpTextFieldExample class >> title [

	^ 'Text Field Example'
]

{ #category : #initialization }
SpTextFieldExample >> connectPresenters [
	
	textField whenTextChangedDo: [ :text | 
		self class environment
			at: text asSymbol
			ifPresent: [ :class | methodBrowser messages: class methods ]
			ifAbsent: [ methodBrowser messages: #() ] ]
]

{ #category : #api }
SpTextFieldExample >> initialExtent [
	^ 900@550
]

{ #category : #initialization }
SpTextFieldExample >> initializePresenters [

	self flag: #TODO. "Using reference to suspend a dependency, but is not cool since 
	it makes a hidden depenency. Instead, we better think another example" 
	methodBrowser := self instantiate: (self class environment at: #MessageBrowser).

	textField := self newTextInput.
	textField
		placeholder: 'Enter a class name';
		removeEntryCompletion.

	methodBrowser sortingBlock: [ :a :b | a selector < b selector ]
]

{ #category : #accessing }
SpTextFieldExample >> methodBrowser [

	^ methodBrowser
]

{ #category : #accessing }
SpTextFieldExample >> textField [

	^ textField
]
