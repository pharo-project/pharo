"
I implement collation of objects using their property. Property is defined by selector or one arg block which can be converted directly to my instances:
	#name ascending.
	#name descending.
	[:a | a name] ascending
	[:a | a name] descending
I provide extra setting to specify what to do with undefined properties which value is nil.
	#name ascending undefinedFirst
- will put all objects with nil in name into the begin of order
	#name ascending undefinedLast
- will put all objects with nil in name into the end of order

Look at SortFunction comments for more details.

Internal Representation and Key Implementation Points.

    Instance Variables
	propertyValuable:		<Symbol, BlockClosure>
	undefinedDirection:		<SmallInteger>	1 nil's at first, -1 for nil's at last
"
Class {
	#name : #SortByPropertyFunction,
	#superclass : #SortFunction,
	#instVars : [
		'propertyValuable',
		'undefinedDirection'
	],
	#category : #SortFunctions-Core
}

{ #category : #'instance creation' }
SortByPropertyFunction class >> computedBy: selectorOrOneArgBlock [
	^self new 
		propertyValuable: selectorOrOneArgBlock
]

{ #category : #comparing }
SortByPropertyFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ direction = anObject direction
		and: [ undefinedDirection = anObject undefinedDirection
				and: [ propertyValuable = anObject propertyValuable ] ]
]

{ #category : #evaluating }
SortByPropertyFunction >> collateProperty: aValue1 with: aValue2 [
	^ self hasUndefinedDirection
		ifTrue: [ 
			aValue1 isNil
				ifTrue: [ undefinedDirection * direction ]
				ifFalse: [ 
					aValue2 isNil
						ifTrue: [ undefinedDirection negated * direction ]
						ifFalse: [ aValue1 threeWayCompareTo: aValue2 ] ] ]
		ifFalse: [ aValue1 threeWayCompareTo: aValue2 ]
]

{ #category : #testing }
SortByPropertyFunction >> hasUndefinedDirection [

	^undefinedDirection notNil
]

{ #category : #comparing }
SortByPropertyFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ direction hash
		bitXor: (undefinedDirection hash bitXor: propertyValuable hash)
]

{ #category : #initailize-release }
SortByPropertyFunction >> initialize [
	super initialize.

	self undefinedLast
]

{ #category : #testing }
SortByPropertyFunction >> isUndefinedFirst [

	^undefinedDirection = -1
]

{ #category : #testing }
SortByPropertyFunction >> isUndefinedLast [

	^undefinedDirection = 1
]

{ #category : #accessing }
SortByPropertyFunction >> propertyValuable [
	^ propertyValuable
]

{ #category : #accessing }
SortByPropertyFunction >> propertyValuable: anObject [
	propertyValuable := anObject
]

{ #category : #evaluating }
SortByPropertyFunction >> threeWayCompare: anObject1 with: anObject2 [

	^ self 
		collateProperty: 	(propertyValuable value: anObject1)
		with: (propertyValuable value: anObject2)
]

{ #category : #accessing }
SortByPropertyFunction >> undefinedDirection [
	^ undefinedDirection
]

{ #category : #initailize-release }
SortByPropertyFunction >> undefinedFirst [

	undefinedDirection := -1
]

{ #category : #initailize-release }
SortByPropertyFunction >> undefinedLast [

	undefinedDirection := 1
]
