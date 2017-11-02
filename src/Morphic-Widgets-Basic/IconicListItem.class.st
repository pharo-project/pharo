"
I am a wrapper used to display a morph with ahead an icon.

Here is the way to use me

IconicListItem new
	icon: ThemeIcons current smallOkIcon asMorph;
	morph: 'Accept' asMorph;
	yourself;
	openInWorld
"
Class {
	#name : #IconicListItem,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'icon',
		'morph',
		'originalObject'
	],
	#category : 'Morphic-Widgets-Basic'
}

{ #category : #'instance creation' }
IconicListItem class >> text: aText icon: form [
	| morph |
	
	aText ifNotNil: [ morph := aText asMorph 
		lock;
		yourself ].

	^ self new
		morph:	morph;
		icon: form;
		yourself
]

{ #category : #accessing }
IconicListItem >> enabled: aBoolean [

	self submorphs do: [ :each |
			each enabled: aBoolean ].
]

{ #category : #accessing }
IconicListItem >> icon [
	^ icon
]

{ #category : #accessing }
IconicListItem >> icon: anObject [

	icon ifNotNil: [ self removeMorph: icon ].
	
	icon := anObject asAlphaImageMorph.
	anObject ifNotNil: [ "since the text can be removed" 
		self addMorphFront: icon.
		self adjustLayoutBounds]
	
]

{ #category : #initialization }
IconicListItem >> initialize [

	super initialize.

	self 	
		changeTableLayout;
		listDirection: #leftToRight;
		cellPositioning: #center;
		cellInset: 2;
		borderWidth: 0;
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		extent: 20@16.
]

{ #category : #accessing }
IconicListItem >> morph [
	^ morph
]

{ #category : #accessing }
IconicListItem >> morph: anObject [
	
	morph ifNotNil: [ self removeMorph: morph ].
	morph := anObject.
	morph ifNotNil: [ "since the text can be removed" 
		self addMorphBack: morph.
		self adjustLayoutBounds]
	
]

{ #category : #accessing }
IconicListItem >> originalObject [

	^ originalObject
]

{ #category : #accessing }
IconicListItem >> originalObject: anObject [
	
	originalObject := anObject
]
