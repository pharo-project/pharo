"
Description
--------------------

I am a presenter for a modal with default buttons whose user can close by clicking on the backdrop.
"
Class {
	#name : #SpDemoModal1Presenter,
	#superclass : #SpDemoAbstractModalPresenter,
	#category : #'Spec2-Examples-Demo-Modals'
}

{ #category : #accessing }
SpDemoModal1Presenter >> content [
	^ 'I am a modal demo with default actions buttons. I set closeOnBackdropClick to true (which is the default value) to close the modal if the user click on the backdrop of the modal. This will trigger the cancel action of the modal.'
]

{ #category : #initialization }
SpDemoModal1Presenter >> initializeDialogWindow: aDialog [
	super initializeDialogWindow: aDialog.
	aDialog
		closeOnBackdropClick: true;
		okAction: [ :presenter | self inform: 'Ok action' ];
		cancelAction: [ :presenter | self inform: 'Cancel' ]
]
