"
I am the bridge between an ImagePresenter and a AlphaImageMorph
"
Class {
	#name : #MorphicImageAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #Spec-MorphicAdapters
}

{ #category : #initialization }
MorphicImageAdapter >> adapt: aComposablePresenter [

	super adapt: aComposablePresenter.
	widget
		on: #click
		send: #click
		to: self
]

{ #category : #factory }
MorphicImageAdapter >> buildWidget [
	| alphaImage |
	alphaImage := AlphaImageMorph new.
	alphaImage model: self.
	alphaImage
		getImageSelector: #getImage;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		layout: (self model autoScale 
				ifTrue: [ #scaledAspect ]
				ifFalse: [ #center ]);
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		update: #getImage.
	^ alphaImage
]

{ #category : #private }
MorphicImageAdapter >> click [

	^ self model action value
]

{ #category : #'widget protocol' }
MorphicImageAdapter >> getImage [

	^ self model image
]

{ #category : #'widget protocol' }
MorphicImageAdapter >> switchAutoscale [
	| layoutValue |
	layoutValue := self model autoScale
		ifTrue: [ #scaledAspect ]
		ifFalse: [ #center ].
	self widgetDo: [ :w | w layout: layoutValue ]
]
