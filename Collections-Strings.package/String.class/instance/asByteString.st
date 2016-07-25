converting
asByteString
	"Convert the receiver into a ByteString, if possible"
	"Do not raise an error if it's not possible, since my use case is usually one in which WideStrings may or may not have been mutated to something representable in a ByteString, and we mostly do this to save space if possible. If the percentage of such cases are small, it may be better to use isOctetString check first to avoid creating String instances"	
	^self asOctetString