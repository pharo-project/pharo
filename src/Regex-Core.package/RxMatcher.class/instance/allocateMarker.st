private
allocateMarker
	"Answer an integer to use as an index of the next marker."

	markerCount := markerCount + 1.
	^markerCount