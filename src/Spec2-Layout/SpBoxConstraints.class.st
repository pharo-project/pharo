"
I am the constraints that can be applied to a presenter placed into a box layout.
I can define: 

- expand: the widget will expand to take all the space the layout wants to give it.
- fill: the widget will fill the space taken (this property just has sense if expand=false)
- padding: the padding let before the next presenter.
- width/height: fixed width and height to take. 
"
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
SpBoxConstraints >> height: aNumber [
	"height of component.
	 this just has sense in the case of expand=false"

	self expand: false.
	height := aNumber
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
