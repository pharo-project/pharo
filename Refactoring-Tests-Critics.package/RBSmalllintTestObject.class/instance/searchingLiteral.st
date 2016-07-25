methods
searchingLiteral
	^self printString = #a or: [#() = self printString | ( #() == self printString)]