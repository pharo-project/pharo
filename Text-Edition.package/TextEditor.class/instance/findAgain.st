menu messages
findAgain
	"Find the text-to-find again."
	| where |
	where := self findReplaceService findNextSubstring: self findReplaceService findString inTextMorph: self morph.
	where isNil
		ifTrue: [self flash]
		ifFalse: [ self selectInterval: where].
	^ where
