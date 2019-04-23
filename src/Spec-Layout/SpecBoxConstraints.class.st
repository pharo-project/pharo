Class {
	#name : #SpecBoxConstraints,
	#superclass : #SpecLayoutConstraints,
	#instVars : [
		'expand',
		'fill',
		'padding',
		'placedAtStart',
		'height',
		'width'
	],
	#category : #'Spec-Layout-Box'
}

{ #category : #'as yet unclassified' }
SpecBoxConstraints class >> defaultExpand [

	^ true
]

{ #category : #'as yet unclassified' }
SpecBoxConstraints class >> defaultFill [

	^ true
]

{ #category : #'as yet unclassified' }
SpecBoxConstraints class >> defaultPadding [ 

	^ 0
]

{ #category : #accessing }
SpecBoxConstraints >> bePlacedAtEnd [

	placedAtStart := false
]

{ #category : #accessing }
SpecBoxConstraints >> bePlacedAtStart [

	placedAtStart := true
]

{ #category : #accessing }
SpecBoxConstraints >> expand: aBoolean [

	expand := aBoolean
]

{ #category : #accessing }
SpecBoxConstraints >> fill: aBoolean [

	fill := aBoolean
]

{ #category : #accessing }
SpecBoxConstraints >> height [
	"height of component.
	 this just has sense in the case of expand=false"

	^ height
]

{ #category : #accessing }
SpecBoxConstraints >> height: anObject [
	"height of component.
	 this just has sense in the case of expand=false"

	self expand: false.
	height := anObject
]

{ #category : #initialization }
SpecBoxConstraints >> initialize [ 

	super initialize.
	self bePlacedAtStart
]

{ #category : #testing }
SpecBoxConstraints >> isExpand [
	^ expand ifNil: [ self class defaultExpand ]
]

{ #category : #testing }
SpecBoxConstraints >> isFill [
	^ fill ifNil: [ self class defaultFill ]
]

{ #category : #testing }
SpecBoxConstraints >> isPlacedAtStart [
	
	^ placedAtStart
]

{ #category : #accessing }
SpecBoxConstraints >> padding [
	^ padding ifNil: [ self class defaultPadding ]
]

{ #category : #accessing }
SpecBoxConstraints >> padding: aNumber [

	padding := aNumber
]

{ #category : #accessing }
SpecBoxConstraints >> width [
	"width of component.
	 this just has sense in the case of expand=false"

	^ width
]

{ #category : #accessing }
SpecBoxConstraints >> width: aNumber [
	"width of component.
	 this just has sense in the case of expand=false"

	self expand: false.
	^ width
]
