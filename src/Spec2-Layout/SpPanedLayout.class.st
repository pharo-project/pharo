"
I'm a Paned Layout.
A paned layout is like a Box Layout (it places childen in vertical or horizontal fashion), but it will add a splitter in between, that user can drag to resize the panel.
In exchange, a paned layout can have just two children.
"
Class {
	#name : #SpPanedLayout,
	#superclass : #SpExecutableLayout,
	#instVars : [
		'direction',
		'position'
	],
	#category : #'Spec2-Layout-Paned'
}

{ #category : #'instance creation' }
SpPanedLayout class >> new [ 

	self error: 'Use one of my siblings'
]

{ #category : #'instance creation' }
SpPanedLayout class >> newHorizontal [ 

	^ super new beHorizontal
]

{ #category : #'instance creation' }
SpPanedLayout class >> newVertical [ 

	^ super new beVertical
]

{ #category : #private }
SpPanedLayout >> adapterName [

	^ #PanedAdapter
]

{ #category : #adding }
SpPanedLayout >> add: aName [

	self 
		add: aName 
		withConstraints: [ :constraints | ]
]

{ #category : #adding }
SpPanedLayout >> add: aName withConstraints: aBlock [
	| constraints |
	
	children size = 2 ifTrue: [ 
		self error: 'You can not add more than two paned presenters.' ].

	constraints := self constraintsClass new.
	aBlock value: constraints.
	children 
		at: aName 
		put: constraints
]

{ #category : #initialization }
SpPanedLayout >> beHorizontal [

	direction := SpLayoutDirection horizontal
]

{ #category : #initialization }
SpPanedLayout >> beVertical [

	direction := SpLayoutDirection vertical
]

{ #category : #private }
SpPanedLayout >> constraintsClass [

	^ SpPanedConstraints
]

{ #category : #accessing }
SpPanedLayout >> direction [ 

	^ direction
]

{ #category : #initialization }
SpPanedLayout >> initialize [ 

	super initialize.
	self position: 50 percent
]

{ #category : #testing }
SpPanedLayout >> isHorizontal [ 

	^ self direction = SpLayoutDirection horizontal
]

{ #category : #testing }
SpPanedLayout >> isVertical [ 

	^ self direction = SpLayoutDirection vertical
]

{ #category : #accessing }
SpPanedLayout >> position [
	"Indicates original position of splitter. 
	 It can be nil (then it defaults to 50%).
	 It can be a percentage (e.g. 30 percent)"

	^ position
]

{ #category : #accessing }
SpPanedLayout >> position: anObject [
	position := anObject
]
