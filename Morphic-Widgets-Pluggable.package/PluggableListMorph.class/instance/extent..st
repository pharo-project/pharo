geometry
extent: newExtent
	super extent: newExtent.
	
	"Change listMorph's bounds to the new width. It is either the size
	of the widest list item, or the size of self, whatever is bigger"
	self listMorph width: ((self width max: listMorph hUnadjustedScrollRange) + 20). 
