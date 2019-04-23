"
Description
--------------------

I am a morphic adapter used to show a modal for a ModalWindowPresenter in Morphic.
"
Class {
	#name : #MorphicModalWindowAdapter,
	#superclass : #MorphicDialogWindowAdapter,
	#instVars : [
		'backdropMorph'
	],
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #protocol }
MorphicModalWindowAdapter >> close [
	backdropMorph ifNotNil: #delete.
	backdropMorph := nil
]

{ #category : #testing }
MorphicModalWindowAdapter >> closeOnBackdropClick [
	^ self model closeOnBackdropClick
]

{ #category : #'event handling' }
MorphicModalWindowAdapter >> mouseClick: evt onBackdrop: aMorph [
	self closeOnBackdropClick ifFalse: [ ^ self ].

	self model triggerCancelAction.
	self model close. "Ensure the modal is closed"
	aMorph delete
]

{ #category : #protocol }
MorphicModalWindowAdapter >> open [

	self model windowIsOpening.

	backdropMorph := Morph new
		color: (Color black alpha: 0.7);
		beSticky;
		extent: World extent;
		openInWorld;
		takeKeyboardFocus;
		on: #click send: #mouseClick:onBackdrop: to: self;
		setProperty: #morphicLayerNumber toValue: 2.
	
	self widget
		setProperty: #morphicLayerNumber toValue: 1;
		toggleStickiness;
		center: World clearArea center.
		
	backdropMorph openModal: self widget
]
