geometry
extent: newExtent
	"Change listMorph's bounds to the new width. It is either the size
	of the widest list item, or the size of self, whatever is bigger"

	super extent: newExtent.
	self listMorph width: (self innerBounds width max: listMorph hUnadjustedScrollRange). 
