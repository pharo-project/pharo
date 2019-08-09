"
I am a LabelledContainer specialized to manage a TextInputFieldPresenter.
See LabelledContainer

self example

I provide the accessor input to be more intuitive than subwidget.

I provide shortcuts for the most common methods of the TextInputFieldPresenter api to avoid the use of the DoesNotUnderstand or the writing of long chain of accessors.
"
Class {
	#name : #SpLabelledTextInputPresenter,
	#superclass : #SpLabelledContainerPrestenter,
	#category : #'Spec2-PolyWidgets-Labelled'
}

{ #category : #specs }
SpLabelledTextInputPresenter class >> defaultSpec [
	^ self labelLeft
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> acceptBlock: aBlockWithOneArgument [
	^ self input acceptBlock: aBlockWithOneArgument
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> acceptOnCR: aBoolean [
	^ self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> autoAccept: aBoolean [
	^ self input autoAccept: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> beEncrypted [
	^ self input beEncrypted
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> encrypted: aBoolean [
	^self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> entryCompletion: anEntryCompletion [
	^self input entryCompletion: anEntryCompletion
]

{ #category : #initialization }
SpLabelledTextInputPresenter >> initializeWidgets [
	super initializeWidgets.
	self content: SpTextInputFieldPresenter.
]

{ #category : #accessing }
SpLabelledTextInputPresenter >> input [
	^ self subwidget
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> placeholder: aText [
	^ self input placeholder: aText
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> removeEntryCompletion [
	^self input removeEntryCompletion
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> selectAll [
	^self input selectAll
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> text [
	^self input text
]

{ #category : #'api-shortcuts' }
SpLabelledTextInputPresenter >> text: aText [
	self input text: aText
]

{ #category : #'events-shortcuts' }
SpLabelledTextInputPresenter >> whenTextChanged: aBlock [
	self input whenTextChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpLabelledTextInputPresenter >> whenTextIsAccepted: aBlock [
	self input whenTextIsAccepted: aBlock
]
