"
Description
--------------------

I am a presenter for a modal with custom buttons whose user cannot close by clicking on the backdrop.
"
Class {
	#name : #SpDemoModal2Presenter,
	#superclass : #SpDemoAbstractModalPresenter,
	#category : #'Spec2-Examples-Demo-Modals'
}

{ #category : #accessing }
SpDemoModal2Presenter >> content [
	^ 'I am a modal demo with custom actions buttons.  I set closeOnBackdropClick to false so that the user cannot click on the backdrop to close the modal.'
]

{ #category : #initialization }
SpDemoModal2Presenter >> initializeDialogWindow: aDialog [
	aDialog
		closeOnBackdropClick: false;
		addButton: 'Validate'
			do: [ :presenter | 
			self inform: 'Validate'.
			presenter close ];
		addButton: 'Reset'
			do: [ :presenter | self inform: 'This action does not close the modal' ];
		addButton: 'Cancel'
			do: [ :presenter | 
			self inform: 'Cancel'.
			presenter close ]
]
