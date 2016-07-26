private
responseForFile: filename fromRequest: aRequest
	| file entry size time mimeType fileStream entity response modified |
	file := filename asFileReference.
	entry := file entry.
	size := entry size.
	time := entry modificationTime.
	modified := true.
	aRequest headers
		at: 'If-Modified-Since'
		ifPresent: [ modified := time > (ZnUtils parseHttpDate: (aRequest headers at: 'If-Modified-Since')) ].
	modified
		ifTrue: [ 
			mimeType := ZnMimeType forFilenameExtension: file extension.
			fileStream := file readStream.
			fileStream binary.
			(entity := ZnStreamingEntity type: mimeType length: size) 
				stream: fileStream.
			(response := ZnResponse ok: entity) 
				headers at: 'Modification-Date' put: (ZnUtils httpDate: time).
			(self maxAgeFor: mimeType) 
				ifNotNil: [ :maxAge | response headers at: 'Cache-Control' put: maxAge ].
			(self expirationDateFor: mimeType)
				ifNotNil: [ :expirationDate | response headers at: 'Expires' put: expirationDate ] ]
		ifFalse: [ response := ZnResponse notModified ].
	^ response