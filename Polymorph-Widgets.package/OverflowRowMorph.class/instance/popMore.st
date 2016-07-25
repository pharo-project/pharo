as yet unclassified
popMore
	"Hide / show the unshown base morphs."

	(self moreMorph isNil or: [self moreMorph owner isNil])
		ifTrue: [self showMore]
		ifFalse: [self hideMore]