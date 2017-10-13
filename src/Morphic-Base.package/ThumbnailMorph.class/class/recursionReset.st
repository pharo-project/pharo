utilities
recursionReset
	"ThumbnailMorph recursionReset"
	"Reset the RecursionDepth counter in case the user interrupted
during a thumbnail being drawn.  Do this just once in a while when no
drawOn: is being called.  tk 9/8/97"

	RecursionDepth := 0.