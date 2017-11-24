"
I'm a Polygon PathShape (see PathShape).
"
Class {
	#name : #Polygon,
	#superclass : #PathShape,
	#category : #'Graphics-Shapes'
}

{ #category : #testing }
Polygon >> containsPoint: aPoint [
	"Answer whether the receiver contains the given point."

	|wind|
	(self basicContainsPoint: aPoint) ifFalse: [^false].
	wind := 0.
	self segmentsDo: [:p1 :p2 |
		p1 y <= aPoint y
			ifTrue: [p2 y = aPoint y
						ifTrue: [(aPoint directionToLineFrom: p1 to: p2) = 0
							ifTrue: [^true]]
						ifFalse: [(p2 y > aPoint y and: [(aPoint directionToLineFrom: p1 to: p2) > 0])
							ifTrue: [wind := wind + 1]]]
			ifFalse: [p2 y = aPoint y
						ifTrue: [(aPoint directionToLineFrom: p1 to: p2) = 0
								ifTrue: [^true]].
					(p2 y < aPoint y and: [(aPoint directionToLineFrom: p1 to: p2) < 0])
							ifTrue: [wind := wind - 1]]].
	^wind ~= 0
]

{ #category : #enumerating }
Polygon >> segmentsDo: aBlock [
	"Evaluate the two-argument block with each vertex and its successor."

	self vertices size < 2 ifTrue: [^self].
	super segmentsDo: aBlock.
	aBlock
		value: self vertices last
		value: self vertices first
]
