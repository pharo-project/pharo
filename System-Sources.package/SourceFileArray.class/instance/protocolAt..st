public - string reading
protocolAt: sourcePointer
	"Answer the protocol for a given method, retrieved from the sources or changes file. Answer unfound protocol if no protocol is available"
				
	^ self protocolAt: sourcePointer for: self sourceDataPointers