"
Description
--------------------

I am a morphic adapter used to show a modal for a ModalWindowPresenter in Morphic.
"
Class {
	#name : #SpMorphicModalWindowAdapter,
	#superclass : #SpMorphicDialogWindowAdapter,
	#instVars : [
		'backdropMorph'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #protocol }
SpMorphicModalWindowAdapter >> close [
	backdropMorph ifNotNil: #delete.
	backdropMorph := nil
]

{ #category : #testing }
SpMorphicModalWindowAdapter >> closeOnBackdropClick [
	^ self model closeOnBackdropClick
]

{ #category : #'event handling' }
SpMorphicModalWindowAdapter >> mouseClick: evt onBackdrop: aMorph [
	self closeOnBackdropClick ifFalse: [ ^ self ].

	self model triggerCancelAction.
	self model close. "Ensure the modal is closed"
	aMorph delete
]

{ #category : #protocol }
SpMorphicModalWindowAdapter >> open [
	self model windowIsOpening.

	backdropMorph := FullscreenMorph new
		color: self theme modalBackdropColor;
		on: #click send: #mouseClick:onBackdrop: to: self;
		openInWorld;
		yourself.

	self widget
		toggleStickiness;
		openCenteredInWorld.

	[ self widget world notNil ] whileTrue: [ self widget outermostWorldMorph doOneCycle ]
]
