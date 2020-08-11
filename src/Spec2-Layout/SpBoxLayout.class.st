"
I am a layout that will arrange presenters in a box, vertically or horizontally.
My elements can be added at the begining or at the end of the box (see add: and addLast: method families).
This layout can be created with certain properties: 

- borderWidth: the border to let empty before placing the presenters. 
- spacing: the space to let between presenters.
- homogeneous: if homogeneous, the layout will try to give same space to each presenter included.
- hAlign/vAlign: how the presenters will be aligned in the layout.
"
Class {
	#name : #SpBoxLayout,
	#superclass : #SpExecutableLayout,
	#traits : 'SpTAlignable',
	#classTraits : 'SpTAlignable classTrait',
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

	self error: 'Use either #newHorizontal or #newVertical'
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
	self announceChildAdded: aPresenter.
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
SpBoxLayout >> removeAll [

	children removeAll.
	adapter ifNotNil: [ :anAdapter | anAdapter removeAll ]
]

{ #category : #adding }
SpBoxLayout >> replace: aPresenter with: otherPresenter [
	"Replace aPresenter with otherPresenter. 
	 Original constraints will be preserved."
	
	self 
		replace: aPresenter 
		with: otherPresenter
		withConstraints: (self constraintsFor: aPresenter)
]

{ #category : #adding }
SpBoxLayout >> replace: aPresenter with: otherPresenter withConstraints: constraints [
	"Replace aPresenter with otherPresenter"
	
	children
		replaceKey: aPresenter
		with: otherPresenter.

	self withAdapterDo: [ :anAdapter |
		anAdapter 
			replace: aPresenter 
			with: otherPresenter
			withConstraints: constraints ]
]

{ #category : #accessing }
SpBoxLayout >> spacing [
	^ spacing
]

{ #category : #accessing }
SpBoxLayout >> spacing: aNumber [

	spacing := aNumber
]
