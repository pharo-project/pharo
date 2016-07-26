compatibility
contentType: type content: content url: url
	^ self 
		contents: content 
		mimeType: type asZnMimeType 
		uri: url