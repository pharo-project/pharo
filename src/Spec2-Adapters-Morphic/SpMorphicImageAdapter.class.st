"
I am the bridge between an ImagePresenter and a AlphaImageMorph
"
Class {
	#name : #SpMorphicImageAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicImageAdapter >> adapt: aPresenter [

	super adapt: aPresenter.
	widget
		on: #click
		send: #click
		to: self
]

{ #category : #factory }
SpMorphicImageAdapter >> buildWidget [
	| alphaImage |
	alphaImage := AlphaImageMorph new.
	alphaImage model: self.
	alphaImage
		getImageSelector: #getImage;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		layout: self layoutValue;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		update: #getImage.

	self model
		whenImageChangeDo: [ alphaImage image: (self getImage ifNil: [ Form extent: 1 @ 1 depth: 32 ]) ];
		whenAutoScaleChangeDo: [ widget layout: self layoutValue ].

	^ alphaImage
]

{ #category : #private }
SpMorphicImageAdapter >> click [

	^ self model action value
]

{ #category : #'widget protocol' }
SpMorphicImageAdapter >> getImage [

	^ self model image
]

{ #category : #accessing }
SpMorphicImageAdapter >> image [
	
	^ widget scaledImage
]

{ #category : #'widget protocol' }
SpMorphicImageAdapter >> layoutValue [
	^ self model autoScale
		ifTrue: [ #scaledAspect ]
		ifFalse: [ #center ]
]

{ #category : #'widget protocol' }
SpMorphicImageAdapter >> switchAutoscale [
	self widgetDo: [ :w | w layout: self layoutValue ]
]
