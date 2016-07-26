accessing
after: target ifAbsent: exceptionBlock
	"Answer the element after target.  Answer the result of evaluation
	the exceptionBlock if target is not in the receiver, or if there are 
	no elements after it."

	| index |
	index := self indexOf: target.
	^ (index = 0 or: [index = self size])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index + 1]