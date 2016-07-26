successor sequences
withNonBodySuccessorsDo: block alreadySeen: set
	"Iterate over me and all my successors only once"

	(set includes: self) ifTrue: [^ self].
	set add: self.
	block value: self.
	self nonBodySuccessorSequences do: [:seq |
		seq ifNotNil: [seq withNonBodySuccessorsDo: block alreadySeen: set]].
