operation
applyAttribute: anAttribute from: startPosition to: endPosition
	| splitStart splitEnd secondSpan |
	(style includes: anAttribute)
		ifTrue: [ ^ self ].
	splitStart := startPosition span == self
		ifTrue: [ startPosition spanPosition ]
		ifFalse: [ 0 ].
	splitEnd := endPosition span == self
		ifTrue: [ endPosition spanPosition ]
		ifFalse: [ self size ].
	secondSpan := splitStart > 0
		ifTrue: [ self splitAt: splitStart ]
		ifFalse: [ self ].
	splitEnd - splitStart < secondSpan size
		ifTrue: [ 
			secondSpan splitAt: splitEnd - splitStart.	"update the position"
			endPosition position: secondSpan endPosition ].
	secondSpan applyAttribute: anAttribute