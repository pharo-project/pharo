accessing
indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	| index |
	index := self findString: sub startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index