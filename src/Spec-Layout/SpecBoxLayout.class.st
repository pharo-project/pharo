Class {
	#name : #SpecBoxLayout,
	#superclass : #SpecExecutableLayout,
	#instVars : [
		'direction',
		'spacing',
		'borderWidth',
		'homogeneous'
	],
	#category : #'Spec-Layout-Box'
}

{ #category : #'instance creation' }
SpecBoxLayout class >> new [ 

	self error: 'Use one of my siblings'
]

{ #category : #'instance creation' }
SpecBoxLayout class >> newHorizontal [ 

	^ super new beHorizontal
]

{ #category : #'instance creation' }
SpecBoxLayout class >> newVertical [ 

	^ super new beVertical
]

{ #category : #private }
SpecBoxLayout >> adapterName [

	^ #BoxAdapter
]

{ #category : #adding }
SpecBoxLayout >> add: aName [

	self 
		add: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #adding }
SpecBoxLayout >> add: aName expand: shouldExpand fill: shouldFill padding: aNumber [

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
]

{ #category : #adding }
SpecBoxLayout >> add: aPresenter withConstraints: aBlock [
	
	super add: aPresenter withConstraints: aBlock.
	adapter ifNotNil: [ :theAdapter |
		theAdapter add: aPresenter constraints: (children at: aPresenter) ]
]

{ #category : #adding }
SpecBoxLayout >> addLast: aName [

	self 
		addLast: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #adding }
SpecBoxLayout >> addLast: aName expand: shouldExpand fill: shouldFill padding: aNumber [

	self 
		addLast: aName 
		withConstraints: [ :constraints |
			constraints 
				expand: shouldExpand;
				fill: shouldFill; 
				padding: aNumber ]
]

{ #category : #adding }
SpecBoxLayout >> addLast: aName withConstraints: aBlock [

	self 
		add: aName 
		withConstraints: [ :constraints |
			constraints bePlacedAtEnd.
			aBlock value: constraints ]
]

{ #category : #accessing }
SpecBoxLayout >> beHomogeneous [

	self homogeneous: true
]

{ #category : #initialization }
SpecBoxLayout >> beHorizontal [

	direction := SpecLayoutDirection horizontal
]

{ #category : #accessing }
SpecBoxLayout >> beNotHomogeneous [

	self homogeneous: false
]

{ #category : #initialization }
SpecBoxLayout >> beVertical [

	direction := SpecLayoutDirection vertical
]

{ #category : #accessing }
SpecBoxLayout >> borderWidth [
	^ borderWidth
]

{ #category : #accessing }
SpecBoxLayout >> borderWidth: anObject [
	borderWidth := anObject
]

{ #category : #private }
SpecBoxLayout >> constraintsClass [

	^ SpecBoxConstraints
]

{ #category : #accessing }
SpecBoxLayout >> direction [ 

	^ direction
]

{ #category : #private }
SpecBoxLayout >> homogeneous: aBoolean [

	homogeneous := aBoolean
]

{ #category : #initialization }
SpecBoxLayout >> initialize [

	super initialize.
	self beNotHomogeneous.
	self spacing: 0.
	self borderWidth: 0
]

{ #category : #testing }
SpecBoxLayout >> isHomogeneous [

	^ homogeneous
]

{ #category : #testing }
SpecBoxLayout >> isHorizontal [ 

	^ self direction = SpecLayoutDirection horizontal
]

{ #category : #testing }
SpecBoxLayout >> isVertical [ 

	^ self direction = SpecLayoutDirection vertical
]

{ #category : #removing }
SpecBoxLayout >> remove: aPresenter [

	children removeKey: aPresenter.
	adapter ifNotNil: [ :theAdapter | theAdapter remove: aPresenter ]
]

{ #category : #accessing }
SpecBoxLayout >> spacing [
	^ spacing
]

{ #category : #accessing }
SpecBoxLayout >> spacing: aNumber [

	spacing := aNumber
]
