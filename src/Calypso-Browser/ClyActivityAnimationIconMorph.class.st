"
I implement rotated icon anomation.
I use TabMorph animation icons (#refreshIcon1, #refreshIcon2, ..) to switch rotating splines at every UI step.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	progress:		<Integer>
	progressIconMorph:		<ImageMorph>
"
Class {
	#name : #ClyActivityAnimationIconMorph,
	#superclass : #Morph,
	#instVars : [
		'progressIconMorph',
		'progress'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #'instance creation' }
ClyActivityAnimationIconMorph class >> label: aString [
	^self new 
		addLabel: aString;
		yourself
]

{ #category : #initialization }
ClyActivityAnimationIconMorph >> addLabel: aStringOrMorph [

	self addMorphBack: aStringOrMorph asMorph

	
]

{ #category : #acccessing }
ClyActivityAnimationIconMorph >> currentIcon [
	^TabMorph perform: (#refreshIcon, progress asString) asSymbol
]

{ #category : #initialization }
ClyActivityAnimationIconMorph >> defaultColor [
	^Color transparent
]

{ #category : #initialization }
ClyActivityAnimationIconMorph >> initialize [
	super initialize.
	self 
		changeTableLayout;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		listDirection: #leftToRight;
		cellInset: 2.
	progress := 1.
	progressIconMorph := self currentIcon asMorph.
	self addMorphBack: progressIconMorph.
	self extent: progressIconMorph extent.
]

{ #category : #stepping }
ClyActivityAnimationIconMorph >> step [
	progress := progress + 1.
	progress > 10 ifTrue: [ progress := 1 ].
	
	progressIconMorph form: self currentIcon
]

{ #category : #stepping }
ClyActivityAnimationIconMorph >> stepTime [
	
	^ 100 "milliseconds"
]
