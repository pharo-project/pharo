accessing
subexpressions: subIndex
	"Answer an array of all matches of the subexpression at the given index.
	The answer is always an array; it is empty if there are no matches."

	| originalPosition startPositions stopPositions reply |
	originalPosition := stream position.
	startPositions := self subBeginning: subIndex.
	stopPositions := self subEnd: subIndex.
	(startPositions isEmpty or: [stopPositions isEmpty]) ifTrue: [^Array new].
	reply := OrderedCollection new.
	startPositions with: stopPositions do:
		[:start :stop |
		stream position: start.
		reply add: (stream next: stop - start)].
	stream position: originalPosition.
	^reply asArray