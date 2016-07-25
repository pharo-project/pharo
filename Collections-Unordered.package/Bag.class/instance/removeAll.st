removing
removeAll
	"Implementation Note: as contents will be overwritten, a shallowCopy of self would be modified.
	An alternative implementation preserving capacity would be to create a new contents:
	self setContents: (self class contentsClass new: contents size)."
	
	contents removeAll