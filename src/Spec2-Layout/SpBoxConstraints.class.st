Class {
	#name : #SpBoxConstraints,
	#superclass : #SpLayoutConstraints,
	#instVars : [
		'expand',
		'fill',
		'padding',
		'placedAtStart',
		'height',
		'width'
	],
	#category : #'Spec2-Layout-Box'
}

{ #category : #defaults }
SpBoxConstraints class >> defaultExpand [

	^ true
]

{ #category : #defaults }
SpBoxConstraints class >> defaultFill [

	^ true
]

{ #category : #defaults }
SpBoxConstraints class >> defaultPadding [ 

	^ 0
]

{ #category : #accessing }
SpBoxConstraints >> bePlacedAtEnd [

	placedAtStart := false
]

{ #category : #accessing }
SpBoxConstraints >> bePlacedAtStart [

	placedAtStart := true
]

{ #category : #accessing }
SpBoxConstraints >> expand: aBoolean [

	expand := aBoolean
]

{ #category : #accessing }
SpBoxConstraints >> fill: aBoolean [

	fill := aBoolean
]

{ #category : #accessing }
SpBoxConstraints >> height [
	"height of component.
	 this just has sense in the case of expand=false"

	^ height
]

{ #category : #accessing }
SpBoxConstraints >> height: anObject [
	"height of component.
	 this just has sense in the case of expand=false"

	self expand: false.
	height := anObject
]

{ #category : #initialization }
SpBoxConstraints >> initialize [ 

	super initialize.
	self bePlacedAtStart
]

{ #category : #testing }
SpBoxConstraints >> isExpand [
	^ expand ifNil: [ self class defaultExpand ]
]

{ #category : #testing }
SpBoxConstraints >> isFill [
	^ fill ifNil: [ self class defaultFill ]
]

{ #category : #testing }
SpBoxConstraints >> isPlacedAtStart [
	
	^ placedAtStart
]

{ #category : #accessing }
SpBoxConstraints >> padding [
	^ padding ifNil: [ self class defaultPadding ]
]

{ #category : #accessing }
SpBoxConstraints >> padding: aNumber [

	padding := aNumber
]

{ #category : #accessing }
SpBoxConstraints >> width [
	"width of component.
	 this just has sense in the case of expand=false"

	^ width
]

{ #category : #accessing }
SpBoxConstraints >> width: aNumber [
	"width of component.
	 this just has sense in the case of expand=false"

	self expand: false.
	width := aNumber
]
