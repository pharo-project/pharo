structure
allOwners
	"Return the owners of the reciever"

	^ Array streamContents: [:strm | self allOwnersDo: [:m | strm nextPut: m]]