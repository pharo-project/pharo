private
handleGetMczEntry: request
	| name file stream entity |
	name := request uri firstPathSegment.
	file := self directory / name.
	^ ((self isValidMczName: name) and: [ file exists ]) 
		ifTrue: [ |  |
			(stream := file readStream)
				binary.
			(entity := ZnStreamingEntity type: ZnMimeType applicationOctetStream length: file size)
				stream: stream.
			ZnResponse ok: entity ]
		ifFalse: [ ZnResponse notFound: request uri ]