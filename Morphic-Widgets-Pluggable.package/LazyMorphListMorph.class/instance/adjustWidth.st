as yet unclassified
adjustWidth
	"private.  Adjust our height to match the length of the underlying list"
	
	self width: (listSource innerBounds width max: self hUnadjustedScrollRange). 