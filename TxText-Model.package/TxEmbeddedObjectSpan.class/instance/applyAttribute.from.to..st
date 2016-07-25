attributes
applyAttribute: anAttribute from: startPosition to: endPosition
	"do not apply only if start position is > 1 or end < 1"
	
	(startPosition span == self and: [ startPosition spanPosition > 0 ]) ifTrue: [ ^ self ].
	(endPosition span == self and: [ endPosition spanPosition = 0 ]) ifTrue: [ ^ self ].
	self applyAttribute: anAttribute.
