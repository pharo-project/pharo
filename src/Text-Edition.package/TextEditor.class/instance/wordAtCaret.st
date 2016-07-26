accessing
wordAtCaret

	^self paragraph text asString wordBefore: self startIndex - 1
