successor sequences
withAllSuccessorsDo: block
	"Iterate over me and all my successors only once"

	self withAllSuccessorsDo: block alreadySeen: IdentitySet new