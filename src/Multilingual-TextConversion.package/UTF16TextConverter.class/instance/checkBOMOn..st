private
checkBOMOn: aStream
	"Write bom if applicable, and we are at beginning of stream"
	(self useByteOrderMark and: [byteOrderMarkDone not]) ifTrue: [
		aStream position = 0 ifTrue: [self next16BitValue: (16rFEFF) toStream: aStream].
		byteOrderMarkDone := true.
	].