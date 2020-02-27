"
I am a grid layout. 
As my name sais, I can place my elements in a grid, following some constraints: 

- a position is mandatory
- a span can be added if desired

A grid layout is tipically used while doing ""form-like"" presenters, for example, we can 
do something like this: 

	--------------------------
	| Name:      | ######### |
	--------------------------
	| Password:  | ######### |
	--------------------------
	|       [Accept] [Cancel]| 
	--------------------------

You can achieve this in several ways, but a simple one would be: 

SpecGridLayout new 
	add: 'Name:' 			at: 1@1;
	add: #nameText 			at: 2@1
	add: 'Password:' 		at: 1@2;
	add: #passwordText 	at: 2@2;
	add: (SpecBoxLayout newHorizontal
			addLast: #acceptButton;
			addLast: #cancelButton;
			yourself)
		at: 1@3 
		span: 2@1;
	yourself
"
Class {
	#name : #SpGridLayout,
	#superclass : #SpExecutableLayout,
	#instVars : [
		'borderWidth',
		'columnSpacing',
		'rowSpacing',
		'rowConstraints',
		'columnConstraints',
		'columnHomogeneous',
		'rowHomogeneous'
	],
	#category : #'Spec2-Layout-Grid'
}

{ #category : #initialization }
SpGridLayout class >> defaultBorderWidth [

	^ 20
]

{ #category : #initialization }
SpGridLayout class >> defaultColumnSpacing [

	^ 10
]

{ #category : #initialization }
SpGridLayout class >> defaultRowSpacing [

	^ 5
]

{ #category : #private }
SpGridLayout >> adapterName [

	^ #GridAdapter
]

{ #category : #adding }
SpGridLayout >> add: aName at: aPoint [

	^ self 
		add: aName 
		at: aPoint
		span: 1@1
]

{ #category : #adding }
SpGridLayout >> add: aName at: aPoint span: spanPoint [

	^ self 
		add: aName 
		withConstraints: [ :constraints |
			constraints 
				position: aPoint;
				span: spanPoint ]
]

{ #category : #accessing }
SpGridLayout >> beColumnHomogeneous [

	self columnHomogeneous: true
]

{ #category : #accessing }
SpGridLayout >> beColumnNotHomogeneous [

	self columnHomogeneous: false
]

{ #category : #accessing }
SpGridLayout >> beRowHomogeneous [

	self rowHomogeneous: true
]

{ #category : #accessing }
SpGridLayout >> beRowNotHomogeneous [

	self rowHomogeneous: false
]

{ #category : #accessing }
SpGridLayout >> borderWidth [

	^ borderWidth ifNil: [ self class defaultBorderWidth ]
]

{ #category : #accessing }
SpGridLayout >> borderWidth: anObject [
	borderWidth := anObject
]

{ #category : #accessing }
SpGridLayout >> column: aNumber withConstraints: aBlock [
	| constraints |

	constraints := SpGridAxisConstraints new.
	aBlock value: constraints.
	columnConstraints 
		at: aNumber
		put: constraints
]

{ #category : #accessing }
SpGridLayout >> columnConstraintsAt: columnNumber [

	^ columnConstraints 
		at: columnNumber 
		ifAbsent: [ nil ]
]

{ #category : #private }
SpGridLayout >> columnHomogeneous: anObject [
	columnHomogeneous := anObject
]

{ #category : #accessing }
SpGridLayout >> columnSpacing [

	^ columnSpacing ifNil: [ self class defaultColumnSpacing ]
]

{ #category : #accessing }
SpGridLayout >> columnSpacing: anObject [
	columnSpacing := anObject
]

{ #category : #private }
SpGridLayout >> constraintsClass [

	^ SpGridConstraints
]

{ #category : #initialization }
SpGridLayout >> initialize [ 

	super initialize.
	self beColumnHomogeneous.
	self beRowHomogeneous.
	columnConstraints := Dictionary new.
	rowConstraints := Dictionary new.
]

{ #category : #testing }
SpGridLayout >> isColumnHomogeneous [
	^ columnHomogeneous
]

{ #category : #testing }
SpGridLayout >> isRowHomogeneous [
	^ rowHomogeneous
]

{ #category : #accessing }
SpGridLayout >> row: aNumber withConstraints: aBlock [
	| constraints |

	constraints := SpGridAxisConstraints new.
	aBlock value: constraints.
	rowConstraints 
		at: aNumber
		put: constraints
]

{ #category : #accessing }
SpGridLayout >> rowConstraintsAt: rowNumber [

	^ rowConstraints 
		at: rowNumber 
		ifAbsent: [ nil ]
]

{ #category : #private }
SpGridLayout >> rowHomogeneous: anObject [
	rowHomogeneous := anObject
]

{ #category : #accessing }
SpGridLayout >> rowSpacing [

	^ rowSpacing ifNil: [ self class defaultRowSpacing ]
]

{ #category : #accessing }
SpGridLayout >> rowSpacing: anObject [
	rowSpacing := anObject
]
