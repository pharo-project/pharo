operations
deleteAll
	"Delete this directory and all children of it, raise an error if the file does not exist."
	DeleteVisitor delete: self resolve