accessing
selectionIntervalFor: aString 
	| interval |
	self searchStrings isEmpty ifTrue: [^nil].
	interval := self selectionParseTreeIntervalFor: aString.
	interval notNil ifTrue: [^interval].
	self searchStrings do: 
			[:each | 
			| search index |
			search := each isSymbol ifTrue: [each keywords first] ifFalse: [each].
			index := aString indexOfSubCollection: search startingAt: 1.
			index > 0 ifTrue: [^index to: index + search size - 1]].
	^nil