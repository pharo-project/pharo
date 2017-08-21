delegated
canonicalize
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"

	^ self withPath: self resolve path canonicalize