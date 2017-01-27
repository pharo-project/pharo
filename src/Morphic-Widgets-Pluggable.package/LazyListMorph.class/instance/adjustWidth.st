drawing
adjustWidth
	"private.  Adjust our width to match that of the underlying list"
	self width: ((listSource width max: self hUnadjustedScrollRange) + 20). 
