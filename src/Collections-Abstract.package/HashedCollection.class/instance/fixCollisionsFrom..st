private
fixCollisionsFrom: start
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."
	
	self subclassResponsibility