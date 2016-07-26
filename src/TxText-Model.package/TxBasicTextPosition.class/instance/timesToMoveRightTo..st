private
timesToMoveRightTo: aPosition
	"Private.
	Assuming that receiver <= aPosition, answer the number of times
	to move receiver right to reach the given position.
	"

	| result mySpan targetSpan |
	mySpan := self span.
	targetSpan := aPosition span.
	result := 0.
	mySpan == targetSpan
		ifTrue: [ ^ aPosition spanPosition - self spanPosition ].
	mySpan
		do: [ :each | 
			each == targetSpan
				ifTrue: [ ^ result + aPosition spanPosition ]
				ifFalse: [ 
					result := each == mySpan
						ifTrue: [ mySpan size - self spanPosition ]
						ifFalse: [ result + each size ] ] ].
	self invalidPosition