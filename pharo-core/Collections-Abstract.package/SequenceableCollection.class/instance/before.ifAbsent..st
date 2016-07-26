accessing
before: target ifAbsent: exceptionBlock
	"Answer the receiver's element immediately before target. Answer
	the result of evaluating the exceptionBlock if target is not an element
	of the receiver, or if there are no elements before it."

	| index |
	index := self indexOf: target.
	^ (index = 0 or: [index = 1])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index - 1]