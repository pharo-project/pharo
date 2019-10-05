"
I'm a layout to show presenters in an ordered box. 
Box can be horizontal or vertical and presenters will be ordered top to down or left to right following direction decided. 
The basic message to add presenters is: #add:expand:fill:padding: (see comment there to understand what the arguments can be). 
To make things easier, I add some basic convenience methods: #add:, #add:expand:

Also, I can add presenters aligned to the bottom in case of vertical layouts or right for horizontal layouts.

Example: 

SpBoxLayout new
	add: 'Some header' expand: false fill: false padding: 5;
	add: #content;
	addLast: 'Some footer' expand: false fill: false padding: 5;
	yourself
"
Class {
	#name : #SpBoxLayout,
	#superclass : #SpExecutableLayout,
	#instVars : [
		'direction',
		'spacing',
		'borderWidth',
		'homogeneous'
	],
	#category : #'Spec2-Layout-Box'
}

{ #category : #'instance creation' }
SpBoxLayout class >> new [ 

	self error: 'Use one of my siblings'
]

{ #category : #'instance creation' }
SpBoxLayout class >> newHorizontal [ 

	^ super new beHorizontal
]

{ #category : #'instance creation' }
SpBoxLayout class >> newVertical [ 

	^ super new beVertical
]

{ #category : #private }
SpBoxLayout >> adapterName [

	^ #BoxAdapter
]

{ #category : #adding }
SpBoxLayout >> add: aName [

	self 
		add: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #adding }
SpBoxLayout >> add: aName expand: shouldExpand [

	^ self 
		add: aName 
		withConstraints: [ :constraints | 
			constraints expand: shouldExpand ]
]

{ #category : #adding }
SpBoxLayout >> add: aName expand: shouldExpand fill: shouldFill padding: aNumber [
"
aName 		- the presenter to be added to box
expand 	- true if the new child is to be given extra space allocated to box . 
			  The extra space will be divided evenly between all children that use this option
fill 		- true if space given to child by the expand option is actually allocated to child , 
			  rather than just padding it. This parameter has no effect if expand is set to false. 
padding 	- extra space in pixels to put between this child and its neighbors, over and above 
			  the global amount specified by “spacing” property. If child is a widget at one of 
			  the reference ends of box , then padding pixels are also put between child and the
			  reference edge of box"

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
]

{ #category : #adding }
SpBoxLayout >> add: aPresenter height: anInteger [ 
	self add: aPresenter withConstraints: [:constraints | constraints height: anInteger ].
]

{ #category : #adding }
SpBoxLayout >> add: aPresenter width: anInteger [
	self
		add: aPresenter
		withConstraints: [ :constraints | constraints width: anInteger ].
]

{ #category : #adding }
SpBoxLayout >> add: aPresenter withConstraints: aBlock [
	
	super add: aPresenter withConstraints: aBlock.
	adapter ifNotNil: [ :theAdapter |
		theAdapter add: aPresenter constraints: (children at: aPresenter) ]
]

{ #category : #adding }
SpBoxLayout >> addLast: aName [

	self 
		addLast: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #adding }
SpBoxLayout >> addLast: aName expand: shouldExpand [

	self 
		addLast: aName 
		withConstraints: [ :constraints | 
			constraints expand: shouldExpand ]
]

{ #category : #adding }
SpBoxLayout >> addLast: aName expand: shouldExpand fill: shouldFill padding: aNumber [

	self 
		addLast: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
]

{ #category : #adding }
SpBoxLayout >> addLast: aName withConstraints: aBlock [

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints bePlacedAtEnd.
			aBlock value: constraints ]
]

{ #category : #accessing }
SpBoxLayout >> beHomogeneous [

	self homogeneous: true
]

{ #category : #initialization }
SpBoxLayout >> beHorizontal [

	direction := SpLayoutDirection horizontal
]

{ #category : #accessing }
SpBoxLayout >> beNotHomogeneous [

	self homogeneous: false
]

{ #category : #initialization }
SpBoxLayout >> beVertical [

	direction := SpLayoutDirection vertical
]

{ #category : #accessing }
SpBoxLayout >> borderWidth [
	^ borderWidth
]

{ #category : #accessing }
SpBoxLayout >> borderWidth: anObject [
	borderWidth := anObject
]

{ #category : #private }
SpBoxLayout >> constraintsClass [

	^ SpBoxConstraints
]

{ #category : #accessing }
SpBoxLayout >> direction [ 

	^ direction
]

{ #category : #private }
SpBoxLayout >> homogeneous: aBoolean [

	homogeneous := aBoolean
]

{ #category : #initialization }
SpBoxLayout >> initialize [

	super initialize.
	self beNotHomogeneous.
	self spacing: 0.
	self borderWidth: 0
]

{ #category : #testing }
SpBoxLayout >> isHomogeneous [

	^ homogeneous
]

{ #category : #testing }
SpBoxLayout >> isHorizontal [ 

	^ self direction = SpLayoutDirection horizontal
]

{ #category : #testing }
SpBoxLayout >> isVertical [ 

	^ self direction = SpLayoutDirection vertical
]

{ #category : #removing }
SpBoxLayout >> remove: aPresenter [

	children removeKey: aPresenter.
	adapter ifNotNil: [ :theAdapter | theAdapter remove: aPresenter ]
]

{ #category : #removing }
SpBoxLayout >> removeAll [

	children removeAll.
	adapter ifNotNil: [ :anAdapter | anAdapter removeAll ]
]

{ #category : #accessing }
SpBoxLayout >> spacing [
	^ spacing
]

{ #category : #accessing }
SpBoxLayout >> spacing: aNumber [

	spacing := aNumber
]
