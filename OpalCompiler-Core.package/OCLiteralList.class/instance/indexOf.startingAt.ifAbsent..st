accessing
indexOf: anElement startingAt: start ifAbsent: exceptionBlock

	start to: self size do:
		[:index | ((self at: index) literalEqual: anElement) ifTrue: [^ index]].
	^ exceptionBlock value