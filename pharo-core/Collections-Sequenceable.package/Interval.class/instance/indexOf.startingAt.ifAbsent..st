accessing
indexOf: anElement startingAt: startIndex ifAbsent: exceptionBlock
	"startIndex is an positive integer, the collection index where the search is started."
	"during the computation of val , floats are only used when the receiver contains floats"

	| index val |
	(anElement isNumber and:[self rangeIncludes: anElement])
		ifFalse: [^ exceptionBlock value].
	val := anElement - self first / self increment.
	val isFloat
		ifTrue: [(val - val rounded) abs * 100000000 < 1
					ifTrue: [index := val rounded + 1]
					ifFalse: [^ exceptionBlock value]]
		ifFalse: [val isInteger
					ifTrue: [index := val + 1]
					ifFalse: [^ exceptionBlock value]].
	"finally, the value of startIndex comes into play:"
	^ (index between: startIndex and: self size)
		ifTrue: [index]
		ifFalse: [exceptionBlock value]