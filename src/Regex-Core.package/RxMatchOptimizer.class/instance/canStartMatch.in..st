accessing
canStartMatch: aCharacter in: aMatcher 
	"Answer whether a match could commence at the given lookahead
	character, or in the current state of <aMatcher>. True answered
	by this method does not mean a match will definitly occur, while false
	answered by this method *does* guarantee a match will never occur."

	aCharacter isNil ifTrue: [^true].
	^testBlock == nil or: [testBlock value: aCharacter value: aMatcher]