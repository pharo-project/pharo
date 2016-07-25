updating
newReplacement
	| t |
	(t := self replaceText asString) ifEmpty: [^ self ].
	(self prevReplacements includes: t)
		ifFalse: [self prevReplacements addFirst: t].
	self prevReplacements size > self maxPreviousListSize
		ifTrue: [self prevReplacements removeLast]