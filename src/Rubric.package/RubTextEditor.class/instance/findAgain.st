menu messages
findAgain
	"Find the text-to-find again."
	| where |
	where := self findReplaceService findNext.
	where isNil
		ifTrue: [self flash]
		ifFalse: [ self selectInterval: where].
	^ where
