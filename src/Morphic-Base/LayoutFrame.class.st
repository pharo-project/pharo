"
I define a transformation frame relative to some rectangle. I'm basic data structure used for graphics.

Do not use fractions: fractionsOrNil offsets: offsetsOrNil or frations: fractionsOrNil if you do not have already  the rectangles that should be passed as arguments. 

If you are creating the rectangles representing the numbers you need, better use the accessors. For example like this.  

	(LayoutFrame identity
			topOffset: topHeight;
			bottomFraction: 0;
			bottomOffset: self buttonsBarHeight;
			leftOffset: -1;
			rightOffset: 1)	






Instance variables:
	leftFraction 
	topFraction 
	rightFraction 
	bottomFraction 	<Float>		The fractional distance (between 0 and 1) to place the morph in its owner's bounds
	leftOffset 
	topOffset 
	rightOffset 
	bottomOffset 	<Integer>	Fixed pixel offset to apply after fractional positioning (e.g., ""10 pixel right of the center of the owner"")
"
Class {
	#name : #LayoutFrame,
	#superclass : #Object,
	#instVars : [
		'leftFraction',
		'leftOffset',
		'topFraction',
		'topOffset',
		'rightFraction',
		'rightOffset',
		'bottomFraction',
		'bottomOffset'
	],
	#category : #Morphic-Base-Layouts
}

{ #category : #'instance creation' }
LayoutFrame class >> identity [
	"by default a layout frame is initialized to represent the identity transformation"
	^ self new
]

{ #category : #comparing }
LayoutFrame >> = aFrame [
	^self species = aFrame species
		and:[ self leftFraction == aFrame leftFraction
		and:[ self leftOffset == aFrame leftOffset
		and:[ self topFraction == aFrame topFraction
		and:[ self topOffset == aFrame topOffset
		and:[ self rightFraction == aFrame rightFraction
		and:[ self rightOffset == aFrame rightOffset
		and:[ self bottomFraction == aFrame bottomFraction
		and:[ self bottomOffset == aFrame bottomOffset]]]]]]]]
]

{ #category : #converting }
LayoutFrame >> asArray [
	 
	^ { leftFraction . 
		topFraction .
		rightFraction .
		bottomFraction .	
		leftOffset .
		topOffset .
		rightOffset .
		bottomOffset } 
]

{ #category : #converting }
LayoutFrame >> asLayoutFrame [
	 ^self
]

{ #category : #accessing }
LayoutFrame >> bottomFraction [
	^ bottomFraction
]

{ #category : #accessing }
LayoutFrame >> bottomFraction: aNumber [
	bottomFraction := aNumber
]

{ #category : #accessing }
LayoutFrame >> bottomFraction: aNumber offset: anInteger [

	bottomFraction := aNumber.
	bottomOffset := anInteger
]

{ #category : #accessing }
LayoutFrame >> bottomOffset [
	^bottomOffset
]

{ #category : #accessing }
LayoutFrame >> bottomOffset: anInteger [
	bottomOffset := anInteger
]

{ #category : #accessing }
LayoutFrame >> bottomRightOffset: aPoint [

	bottomOffset := aPoint y.
	rightOffset := aPoint x.
]

{ #category : #layout }
LayoutFrame >> center: targetMorph with: anotherMorph [
	"Change the receiver to center the targetMorph with anotherMorph. 
	Note the targetMorph is the morph on which the receiver will be applied.
	anotherMorph is often the morph containing the targetMorph. 
	"
	
	self topOffset: (anotherMorph height - targetMorph height) // 2.
	self leftOffset: (anotherMorph width - targetMorph width) // 2.
	
]

{ #category : #accessing }
LayoutFrame >> fractionRectangle [
	"Return a rectangle representing the fraction part of layout frame: i.e left@top corner: right@bottom"
	
	^ leftFraction @ topFraction corner: rightFraction @ bottomFraction 
]

{ #category : #deprecated }
LayoutFrame >> fractions: fractionsOrNil [
	self deprecated: 'Do not use this method. It forces to create spurious objects for nothing.'.
	^ self fractions: fractionsOrNil offsets: nil
]

{ #category : #deprecated }
LayoutFrame >> fractions: fractionsOrNil offsets: offsetsOrNil [

	| fractions offsets |
	self deprecated: 'Do not use this method. It forces to create spurious objects (rectangle or points) for nothing.'.
	fractions := fractionsOrNil ifNil: [0@0 extent: 0@0].
	offsets := offsetsOrNil ifNil: [0@0 extent: 0@0].
	
	^self
		topFraction: fractions top offset: offsets top;
		leftFraction: fractions left offset: offsets left;
		bottomFraction: fractions bottom offset: offsets bottom;
		rightFraction: fractions right offset: offsets right

]

{ #category : #accessing }
LayoutFrame >> fromArray: anArray [
	| str |
	str := anArray readStream.
	
	#( leftFraction: topFraction: rightFraction: bottomFraction: leftOffset: topOffset: rightOffset: bottomOffset: ) do: [:sel |
		str next ifNil: [ ^ self ] ifNotNil: [:value | self perform: sel with: value ]	
	 ]
]

{ #category : #testing }
LayoutFrame >> hasNoOffsets [

	^ leftOffset = 0 and: [rightOffset = 0  and: [ topOffset = 0 and: [ bottomOffset = 0 ]]].
	
]

{ #category : #comparing }
LayoutFrame >> hash [
	^(((((((( self species hash
		+ leftFraction hash) hashMultiply
		+ leftOffset hash) hashMultiply
		+ topFraction hash) hashMultiply
		+ topOffset hash) hashMultiply
		+ rightFraction hash) hashMultiply
		+ rightOffset hash) hashMultiply
		+ bottomFraction hash) hashMultiply
		+ bottomOffset hash) hashMultiply

]

{ #category : #initialization }
LayoutFrame >> initialize [
	"initialize defaults:
	 - all offsets are zero
	 - fraction frame is maximum"
	leftOffset := rightOffset := topOffset := bottomOffset := 0.
	
	leftFraction := topFraction := 0.
	rightFraction := bottomFraction := 1.
]

{ #category : #printing }
LayoutFrame >> isSelfEvaluating [
	^ false
]

{ #category : #layout }
LayoutFrame >> layout: oldBounds in: newBounds [
	"Return the proportional rectangle insetting the given bounds"

	^ self transform: newBounds 
]

{ #category : #accessing }
LayoutFrame >> leftFraction [
	^leftFraction
]

{ #category : #accessing }
LayoutFrame >> leftFraction: aNumber [
	leftFraction := aNumber
]

{ #category : #accessing }
LayoutFrame >> leftFraction: aNumber offset: anInteger [

	leftFraction := aNumber.
	leftOffset := anInteger
]

{ #category : #accessing }
LayoutFrame >> leftOffset [
	^leftOffset
]

{ #category : #accessing }
LayoutFrame >> leftOffset: anInteger [
	leftOffset := anInteger
]

{ #category : #'objects from disk' }
LayoutFrame >> negateBottomRightOffsets [

	bottomOffset := bottomOffset negated.
	rightOffset := rightOffset negated.


]

{ #category : #printing }
LayoutFrame >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' ( '.
	aStream
		print: self leftFraction @ self topFraction; 
		nextPutAll: ' + ';
		print: self leftOffset  @ self topOffset ;
		nextPutAll: 'px corner: ';
		print: self rightFraction @ self bottomFraction; 
		nextPutAll: ' + ';
		print: self rightOffset  @ self bottomOffset ;
		nextPutAll: 'px ) '
]

{ #category : #accessing }
LayoutFrame >> rightFraction [
	^rightFraction
]

{ #category : #accessing }
LayoutFrame >> rightFraction: aNumber [
	rightFraction := aNumber
]

{ #category : #accessing }
LayoutFrame >> rightFraction: aNumber offset: anInteger [

	rightFraction := aNumber.
	rightOffset := anInteger
]

{ #category : #accessing }
LayoutFrame >> rightOffset [
	^rightOffset
]

{ #category : #accessing }
LayoutFrame >> rightOffset: anInteger [
	rightOffset := anInteger
]

{ #category : #accessing }
LayoutFrame >> topFraction [
	^topFraction
]

{ #category : #accessing }
LayoutFrame >> topFraction: aNumber [
	topFraction := aNumber
]

{ #category : #accessing }
LayoutFrame >> topFraction: aNumber offset: anInteger [

	topFraction := aNumber.
	topOffset := anInteger
]

{ #category : #accessing }
LayoutFrame >> topLeftOffset: aPoint [

	topOffset := aPoint y.
	leftOffset := aPoint x.
]

{ #category : #accessing }
LayoutFrame >> topOffset [
	^topOffset
]

{ #category : #accessing }
LayoutFrame >> topOffset: anInteger [
	topOffset := anInteger
]

{ #category : #transforming }
LayoutFrame >> transform: outerReferenceRectangle [
	"Answer a new rectangle which transformed by receiver relative to outerReferenceRectangle.
	If it is not possible to place a outerReferenceRectangle *inside*  this reference rectangle, then result will be zero-width (or zero-height) rectangle. Basically you transform to obtain a subrectangle area inside the reference rectangle"
	
	|  left right top bottom |
	left := (outerReferenceRectangle left + (outerReferenceRectangle width * leftFraction) + leftOffset) rounded asInteger.
	right := (outerReferenceRectangle right - (outerReferenceRectangle width * (1 - rightFraction)) + rightOffset) rounded asInteger.
	top:= (outerReferenceRectangle top + (outerReferenceRectangle height * topFraction) + topOffset ) rounded asInteger.
	bottom:= (outerReferenceRectangle bottom - (outerReferenceRectangle height * (1 - bottomFraction)) + bottomOffset) rounded asInteger. 
	
	right < left ifTrue: [ right := left ].
	bottom < top ifTrue: [ bottom := top ].
	
	^ Rectangle left: left right: right top: top bottom: bottom
]
