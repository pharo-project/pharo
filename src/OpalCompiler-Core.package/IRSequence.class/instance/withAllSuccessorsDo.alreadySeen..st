successor sequences
withAllSuccessorsDo: block alreadySeen: set
	"Iterate over me and all my successors only once"

	(set includes: self) ifTrue: [^ self].
	set add: self.
	block value: self.
	self successorSequences do: [:seq |
		seq ifNotNil: [seq withAllSuccessorsDo: block alreadySeen: set]].
