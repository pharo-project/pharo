"
I am a LabelledContainer specialized to manage a TextInputFieldPresenter.
See LabelledContainer

self example

I provide the accessor input to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the TextInputFieldPresenter api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledTextInput,
	#superclass : #SpLabelledContainer,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpLabelledTextInput class >> defaultSpec [
	^ self labelLeft
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> beEncrypted [
	^ self input beEncrypted
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> encrypted: aBoolean [
	^self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> entryCompletion: anEntryCompletion [
	^self input entryCompletion: anEntryCompletion
]

{ #category : #initialization }
SpLabelledTextInput >> initializePresenters [
	super initializePresenters.
	self content: SpTextInputFieldPresenter.
]

{ #category : #accessing }
SpLabelledTextInput >> input [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> placeholder: aText [
	^ self input placeholder: aText
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> removeEntryCompletion [
	^self input removeEntryCompletion
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> selectAll [
	^self input selectAll
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> text [
	^self input text
]

{ #category : #'api-shortcuts' }
SpLabelledTextInput >> text: aText [
	self input text: aText
]

{ #category : #'api-events' }
SpLabelledTextInput >> whenSubmitDo: aBlock [

	self input whenSubmitDo: aBlock
]

{ #category : #'api-events' }
SpLabelledTextInput >> whenTextChanged: aBlock [

	self input whenTextChanged: aBlock
]

{ #category : #'api-events' }
SpLabelledTextInput >> whenTextIsAccepted: aBlock [

	self input whenTextIsAccepted: aBlock
]
