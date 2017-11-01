"
A PluggableThreePhaseButtonMorph is an extesion of ThreePhaseButtonMorph to make it pluggable

Then a model can be defined, and queried for state, on image, off image and pressed image.
"
Class {
	#name : #PluggableThreePhaseButtonMorph,
	#superclass : #ThreePhaseButtonMorph,
	#instVars : [
		'offImageSelector',
		'onImageSelector',
		'pressedImageSelector',
		'stateSelector'
	],
	#category : 'Morphic-Widgets-Basic-Buttons'
}

{ #category : #'instance creation' }
PluggableThreePhaseButtonMorph class >> on: aModel [

	^ self new 
			target: aModel;
			yourself
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> offImageSelector [

	^ offImageSelector
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> offImageSelector: anObject [ 

	offImageSelector := anObject
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> onImageSelector [

	^ onImageSelector
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> onImageSelector: anObject [ 

	onImageSelector := anObject
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> pressedImageSelector [

	^ pressedImageSelector
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> pressedImageSelector: anObject [ 

	pressedImageSelector := anObject
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> stateSelector [

	^ stateSelector
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> stateSelector: anObject [ 

	stateSelector := anObject
]

{ #category : #accessing }
PluggableThreePhaseButtonMorph >> target: anObject [

	target ifNotNil: [ target removeDependent: self ].
	anObject ifNotNil: [ anObject addDependent: self ].
	target := anObject.
]

{ #category : #updating }
PluggableThreePhaseButtonMorph >> update: aSymbol [

	aSymbol == stateSelector
		ifTrue: [ ^ self updateState ].
	aSymbol == onImageSelector
		ifTrue: [ ^ self updateOnImage ].
	aSymbol == offImageSelector
		ifTrue: [ ^ self updateOffImage ].
	aSymbol == pressedImageSelector
		ifTrue: [ ^ self updatePressedImage ].
				
	^ super update: aSymbol
]

{ #category : #updating }
PluggableThreePhaseButtonMorph >> updateOffImage [

	self offImage: ( target perform: offImageSelector )
]

{ #category : #updating }
PluggableThreePhaseButtonMorph >> updateOnImage [

	self onImage: ( target perform: onImageSelector )
]

{ #category : #updating }
PluggableThreePhaseButtonMorph >> updatePressedImage [

	self pressedImage: ( target perform: pressedImageSelector )
]

{ #category : #updating }
PluggableThreePhaseButtonMorph >> updateState [

	( target perform: stateSelector )
		ifTrue: [ self state: #on ]
		ifFalse:[ self state: #off ]
]
