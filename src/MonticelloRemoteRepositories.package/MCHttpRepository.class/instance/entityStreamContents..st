private
entityStreamContents: aBlock
	"Generate output in a buffer because we need the length"
	
	| stream |
	stream := RWBinaryOrTextStream on: String new.
	aBlock value: stream.
	stream reset.
	^ (ZnStreamingEntity type: ZnMimeType applicationOctetStream)
		stream: stream;
		contentLength: stream size;
		yourself