structure
withAllOwners
	"Return the receiver and all its owners"

	^ Array streamContents: [:strm | self withAllOwnersDo: [:m | strm nextPut: m]]