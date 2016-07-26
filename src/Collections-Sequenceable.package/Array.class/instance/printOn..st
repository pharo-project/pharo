printing
printOn: aStream
	self shouldBePrintedAsLiteral ifTrue: [self printAsLiteralFormOn: aStream. ^ self].
	self isSelfEvaluating ifTrue: [self printAsSelfEvaluatingFormOn: aStream. ^ self].

	super printOn: aStream