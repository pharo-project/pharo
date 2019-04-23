Class {
	#name : #SpecGridLayout,
	#superclass : #SpecExecutableLayout,
	#instVars : [
		'borderWidth',
		'columnSpacing',
		'rowSpacing',
		'columnHomogeneous',
		'rowHomogeneous'
	],
	#category : #'Spec-Layout-Grid'
}

{ #category : #initialization }
SpecGridLayout class >> defaultBorderWidth [

	^ 20
]

{ #category : #initialization }
SpecGridLayout class >> defaultColumnSpacing [

	^ 10
]

{ #category : #initialization }
SpecGridLayout class >> defaultRowSpacing [

	^ 5
]

{ #category : #private }
SpecGridLayout >> adapterName [

	^ #GridAdapter
]

{ #category : #adding }
SpecGridLayout >> add: aName at: aPoint [

	^ self 
		add: aName 
		at: aPoint
		span: 1@1
]

{ #category : #adding }
SpecGridLayout >> add: aName at: aPoint span: spanPoint [

	^ self 
		add: aName 
		withConstraints: [ :constraints |
			constraints 
				position: aPoint;
				span: spanPoint ]
]

{ #category : #accessing }
SpecGridLayout >> beColumnHomogeneous [

	self columnHomogeneous: true
]

{ #category : #accessing }
SpecGridLayout >> beColumnNotHomogeneous [

	self columnHomogeneous: false
]

{ #category : #accessing }
SpecGridLayout >> beRowHomogeneous [

	self rowHomogeneous: false
]

{ #category : #accessing }
SpecGridLayout >> beRowNotHomogeneous [

	self rowHomogeneous: false
]

{ #category : #accessing }
SpecGridLayout >> borderWidth [

	^ borderWidth ifNil: [ self class defaultBorderWidth ]
]

{ #category : #accessing }
SpecGridLayout >> borderWidth: anObject [
	borderWidth := anObject
]

{ #category : #accessing }
SpecGridLayout >> children [

	^ children
]

{ #category : #private }
SpecGridLayout >> columnHomogeneous: anObject [
	columnHomogeneous := anObject
]

{ #category : #accessing }
SpecGridLayout >> columnSpacing [

	^ columnSpacing ifNil: [ self class defaultColumnSpacing ]
]

{ #category : #accessing }
SpecGridLayout >> columnSpacing: anObject [
	columnSpacing := anObject
]

{ #category : #private }
SpecGridLayout >> constraintsClass [

	^ SpecGridConstraints
]

{ #category : #initialization }
SpecGridLayout >> initialize [ 

	super initialize.
	self beColumnHomogeneous.
	self beRowHomogeneous.
]

{ #category : #testing }
SpecGridLayout >> isColumnHomogeneous [
	^ columnHomogeneous
]

{ #category : #testing }
SpecGridLayout >> isRowHomogeneous [
	^ rowHomogeneous
]

{ #category : #private }
SpecGridLayout >> rowHomogeneous: anObject [
	rowHomogeneous := anObject
]

{ #category : #accessing }
SpecGridLayout >> rowSpacing [

	^ rowSpacing ifNil: [ self class defaultRowSpacing ]
]

{ #category : #accessing }
SpecGridLayout >> rowSpacing: anObject [
	rowSpacing := anObject
]
