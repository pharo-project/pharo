encoding
encodeClearedTrailer

	"A cleared trailer is replaced by #NoTrailer, when used for encoding"
	self clear.
	kind := #NoTrailer.
	
	^ self encode