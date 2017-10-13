accessing method dictionary
commentsIn: sourceString

	
	| commentStart nextQuotePos someComments aPos |
	('*"*' match: sourceString) ifFalse: [^#()].
	someComments:= OrderedCollection new.
	sourceString size = 0 ifTrue: [^ someComments].
	aPos:=1.
	nextQuotePos:= 0.
	[commentStart := sourceString findString: '"' startingAt: aPos.
	nextQuotePos:= self nextQuotePosIn: sourceString startingFrom: commentStart.
	(commentStart ~= 0 and: [nextQuotePos >commentStart])] whileTrue: [
		commentStart ~= nextQuotePos ifTrue: [
			someComments add: ((sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"').].
	aPos := nextQuotePos+1].
	^someComments