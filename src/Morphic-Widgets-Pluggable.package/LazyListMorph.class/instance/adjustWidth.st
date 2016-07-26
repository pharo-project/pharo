drawing
adjustWidth
	"private.  Adjust our height to match the length of the underlying list"
	self width: ((listSource width max: self hUnadjustedScrollRange) + 20). 
