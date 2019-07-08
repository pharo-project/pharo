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
	#name : #SpModalWindowPresenter,
	#superclass : #SpDialogWindowPresenter,
	#instVars : [
		'#closeOnBackdropClick => SpObservableSlot'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpModalWindowPresenter class >> adapterName [

	^ #ModalWindowAdapter
]

{ #category : #accessing }
SpModalWindowPresenter >> closeOnBackdropClick [
	"If I am set to true, the user will be able to click on the backdrop to close the modal launching the cancel action."
	
	^ closeOnBackdropClick
]

{ #category : #accessing }
SpModalWindowPresenter >> closeOnBackdropClick: aBoolean [
	"If I am set to true, the user will be able to click on the backdrop to close the modal launching the cancel action."

	closeOnBackdropClick := aBoolean
]

{ #category : #initialization }
SpModalWindowPresenter >> initialize [
	super initialize.
	closeOnBackdropClick := true
]
