"
Description
--------------------

I am a presenter to use a modal. On the contrary of the Dialog presenter, I will block the user to focus any other window.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	closeOnBackdropClick:		<aBooleanValueHolder>		If this variable is at true, the user will be able to close the dialog and launch the close action by clicking on the backdrop.
"
Class {
	#name : #ModalWindowPresenter,
	#superclass : #DialogWindowPresenter,
	#instVars : [
		'closeOnBackdropClick'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
ModalWindowPresenter class >> adapterName [

	^ #ModalWindowAdapter
]

{ #category : #accessing }
ModalWindowPresenter >> closeOnBackdropClick [
	"If I am set to true, the user will be able to click on the backdrop to close the modal launching the cancel action."
	
	^ closeOnBackdropClick value
]

{ #category : #accessing }
ModalWindowPresenter >> closeOnBackdropClick: aBoolean [
	"If I am set to true, the user will be able to click on the backdrop to close the modal launching the cancel action."
	
	closeOnBackdropClick value: aBoolean
]

{ #category : #accessing }
ModalWindowPresenter >> closeOnBackdropClickHolder [
	^ closeOnBackdropClick
]

{ #category : #initialization }
ModalWindowPresenter >> initialize [
	super initialize.
	closeOnBackdropClick := true asValueHolder
]
