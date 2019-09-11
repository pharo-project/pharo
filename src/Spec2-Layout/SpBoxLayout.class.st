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
SpBoxLayout >> add: aName expand: shouldExpand fill: shouldFill padding: aNumber [

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
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

{ #category : #accessing }
SpBoxLayout >> spacing [
	^ spacing
]

{ #category : #accessing }
SpBoxLayout >> spacing: aNumber [

	spacing := aNumber
]
