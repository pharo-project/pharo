testing
shouldBePrintedAsLiteral

	^self class == Array and: [ self allSatisfy: [ :each | each shouldBePrintedAsLiteral ] ]