"
I am used in table layouts to hold temporary values while the layout is being computed.

Instance variables:
	target 		<Morph>		The morph contained in this cell
	cellSize 		<Point>		The size of the cell
	extraSpace 	<nil | Point>	Additional space to add after this cell
	nextCell 	<nil | LayoutCell>	The next cell in the arrangement.

Implementation note:
Both, cellSize and extraSpace contains points where
	x - represents the primary table direction
	y - represents the secondary table direction

"
Class {
	#name : #LayoutCell,
	#superclass : #Object,
	#instVars : [
		'target',
		'cellSize',
		'extraSpace',
		'flags',
		'nextCell'
	],
	#category : #'Morphic-Base-Layouts'
}

{ #category : #accessing }
LayoutCell >> addExtraSpace: aPoint [
	extraSpace 
		ifNil:[extraSpace := aPoint]
		ifNotNil:[extraSpace := extraSpace + aPoint]
]

{ #category : #accessing }
LayoutCell >> cellSize [
	^cellSize
]

{ #category : #accessing }
LayoutCell >> cellSize: aPoint [
	cellSize := aPoint
]

{ #category : #collection }
LayoutCell >> do: aBlock [
	aBlock value: self.
	nextCell ifNotNil:[nextCell do: aBlock].
]

{ #category : #accessing }
LayoutCell >> extraSpace [
	^extraSpace ifNil:[0@0]
]

{ #category : #accessing }
LayoutCell >> extraSpace: aPoint [
	extraSpace := aPoint
]

{ #category : #accessing }
LayoutCell >> flags [
	^flags ifNil: [ 0 ]
]

{ #category : #accessing }
LayoutCell >> hSpaceFill [
	^self flags anyMask: 1
]

{ #category : #accessing }
LayoutCell >> hSpaceFill: aBool [
	flags := aBool ifTrue:[self flags bitOr: 1] ifFalse:[self flags bitClear: 1].

]

{ #category : #collection }
LayoutCell >> inject: thisValue into: binaryBlock [ 
	"Accumulate a running value associated with evaluating the argument, 
	binaryBlock, with the current value of the argument, thisValue, and the 
	receiver as block arguments. For instance, to sum the numeric elements 
	of a collection, aCollection inject: 0 into: [:subTotal :next | subTotal + 
	next]."

	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue
]

{ #category : #accessing }
LayoutCell >> nextCell [
	^nextCell
]

{ #category : #accessing }
LayoutCell >> nextCell: aCell [
	nextCell := aCell
]

{ #category : #accessing }
LayoutCell >> size [
	| n cell |
	n := 0.
	cell := self.
	[cell isNil] whileFalse: 
			[n := n + 1.
			cell := cell nextCell].
	^n
]

{ #category : #accessing }
LayoutCell >> target [
	^target
]

{ #category : #accessing }
LayoutCell >> target: newTarget [
	target := newTarget
]

{ #category : #accessing }
LayoutCell >> vSpaceFill [
	^self flags anyMask: 2
]

{ #category : #accessing }
LayoutCell >> vSpaceFill: aBool [
	flags := aBool ifTrue:[self flags bitOr: 2] ifFalse:[self flags bitClear: 2].

]
