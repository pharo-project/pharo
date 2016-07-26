public - string reading
timeStampAt: sourcePointer
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	^ self timeStampAt: sourcePointer for: self sourceDataPointers