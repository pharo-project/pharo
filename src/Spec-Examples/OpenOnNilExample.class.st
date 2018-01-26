"
I am the component of DynamycSpecExample used for nil.

self example

I display simply an TextInputFieldPresenter, disable to don't be edditable by the user.
"
Class {
	#name : #OpenOnNilExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'text'
	],
	#category : #'Spec-Examples-Wrapper'
}

{ #category : #specs }
OpenOnNilExample class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newRow: #text height: 25;
		yourself.
]

{ #category : #examples }
OpenOnNilExample class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		extent: 200 @ 100;
		openWithSpec.
	^ example
]

{ #category : #initialization }
OpenOnNilExample >> initialize: anInteger [
	text label: anInteger asString
]

{ #category : #initialization }
OpenOnNilExample >> initializeWidgets [
	text := self instantiate: TextInputFieldPresenter.
	text
		text: 'Object is nil';
		enabled: false.
]

{ #category : #accessing }
OpenOnNilExample >> text [
	^ text
]

{ #category : #accessing }
OpenOnNilExample >> title [
	^ 'OpenOnNilExample'
]
