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
		'position',
		'firstChild',
		'secondChild',
		'childrenByPosition'
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
		position: children size + 1
]

{ #category : #adding }
SpPanedLayout >> add: aChild withConstraints: aBlock position: aPosition [
	"Add a child to the layout following the constraints in the block.
	The position is an integer index (1 or 2) indicating whether the child is first or last in the pane.
	For a horizontal layout, index 1 means left, index 2 means right.
	For a vertical layout, index 1 means top, index 2 means bottom."
	| constraints |

	aPosition > 2 ifTrue: [ 
		self error: 'You can not add more than two paned presenters.' ].

	(childrenByPosition at: aPosition) ifNotNil: [ :element |
		self remove: element ].

	constraints := self constraintsClass new.
	aBlock value: constraints.
	
	childrenByPosition at: aPosition put: aChild.
	
	children 
		at: aChild 
		put: constraints.
	
	self announceChildAdded: aChild.
]

{ #category : #adding }
SpPanedLayout >> addFirst: aName [

	self 
		add: aName 
		withConstraints: [ :constraints | ]
		position: 1
]

{ #category : #adding }
SpPanedLayout >> addSecond: aName [

	self 
		add: aName 
		withConstraints: [ :constraints | ]
		position: 2
]

{ #category : #initialization }
SpPanedLayout >> beHorizontal [

	direction := SpLayoutDirection horizontal
]

{ #category : #initialization }
SpPanedLayout >> beVertical [

	direction := SpLayoutDirection vertical
]

{ #category : #accessing }
SpPanedLayout >> children [

	^ childrenByPosition reject: #isNil
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
	self position: 50 percent.
	childrenByPosition := Array new: 2
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

{ #category : #accessing }
SpPanedLayout >> presenter [

	^ adapter ifNotNil: [ adapter presenter ]
]

{ #category : #adding }
SpPanedLayout >> remove: aChild [
	
	children removeKey: aChild.
	self announceChildRemoved: aChild
]
