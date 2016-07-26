handling
freeze
	"freeze the context stack to keep the exception usable outside the catch blocks"

	self freezeUpTo: thisContext