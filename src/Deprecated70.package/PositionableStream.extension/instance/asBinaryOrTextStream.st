*Deprecated70
asBinaryOrTextStream
	"Convert to a stream that can switch between bytes and characters"

	^ (RWBinaryOrTextStream with: self contentsOfEntireFile) reset