"
I am a LabelledContainer specialized to manage a TextInputFieldPresenter.
See LabelledContainer

self example

I provide the accessor input to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the TextInputFieldPresenter api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #LabelledTextInput,
	#superclass : #LabelledContainer,
	#category : #'Spec-PolyWidgets-Labelled'
}

{ #category : #specs }
LabelledTextInput class >> defaultSpec [
	<spec: #default>
	^ super labelLeft
]

{ #category : #example }
LabelledTextInput class >> example [
	| example |
	example := self abstractExample.
	example input ghostText: 'Tilt'.
example
		extent: 400 @ 50;
		openWithSpec.
	^ example
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> acceptBlock: aBlockWithOneArgument [
	^ self input acceptBlock: aBlockWithOneArgument
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> acceptOnCR: aBoolean [
	^ self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> autoAccept: aBoolean [
	^ self input autoAccept: aBoolean
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> beEncrypted [
	^ self input beEncrypted
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> encrypted: aBoolean [
	^self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> entryCompletion: anEntryCompletion [
	^self input entryCompletion: anEntryCompletion
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> ghostText: aText [
	^self input ghostText: aText
]

{ #category : #initialization }
LabelledTextInput >> initializeWidgets [
	super initializeWidgets.
	self content: TextInputFieldPresenter.
]

{ #category : #accessing }
LabelledTextInput >> input [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> removeEntryCompletion [
	^self input removeEntryCompletion
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> selectAll [
	^self input selectAll
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> text [
	^self input text
]

{ #category : #'api-shortcuts' }
LabelledTextInput >> text: aText [
	self input text: aText
]

{ #category : #'events-shortcuts' }
LabelledTextInput >> whenTextChanged: aBlock [
	self input whenTextChanged: aBlock
]

{ #category : #'events-shortcuts' }
LabelledTextInput >> whenTextIsAccepted: aBlock [
	self input whenTextIsAccepted: aBlock
]
