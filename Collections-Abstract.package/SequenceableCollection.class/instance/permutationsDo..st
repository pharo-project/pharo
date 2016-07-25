enumerating
permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	self shallowCopy permutationsStartingAt: 1 do: aBlock