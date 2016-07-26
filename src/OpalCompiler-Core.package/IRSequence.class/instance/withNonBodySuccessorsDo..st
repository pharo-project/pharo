successor sequences
withNonBodySuccessorsDo: block
	"Iterate over me and all my successors only once"

	self withNonBodySuccessorsDo: block alreadySeen: IdentitySet new