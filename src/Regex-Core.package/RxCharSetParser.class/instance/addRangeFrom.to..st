parsing
addRangeFrom: firstChar to: lastChar

	firstChar asInteger > lastChar asInteger ifTrue:
		[RxParser signalSyntaxException: ' bad character range' at: source position].
	elements add: (RxsRange from: firstChar to: lastChar)