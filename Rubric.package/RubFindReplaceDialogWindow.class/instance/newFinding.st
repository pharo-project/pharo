updating
newFinding
	| t |
	(t := self findText asString) ifEmpty: [^ self ].
	(self prevFinds includes: t)
		ifFalse: [self prevFinds addFirst: t].
	self prevFinds size > self maxPreviousListSize
		ifTrue: [self prevFinds removeLast]