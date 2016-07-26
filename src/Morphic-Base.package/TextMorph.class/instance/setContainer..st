containment
setContainer: newContainer
	"Adopt (or abandon) container shape"
	self changed.
	container := newContainer.
	self releaseParagraph