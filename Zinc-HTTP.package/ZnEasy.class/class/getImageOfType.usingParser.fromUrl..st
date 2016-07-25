private
getImageOfType: mimeType usingParser: parserClass fromUrl: urlObject
	| client |
	(client := self client)
		url: urlObject;
		accept: mimeType;
		enforceHttpSuccess: true;
		enforceAcceptContentType: true;
		get.
	^ (parserClass on: client entity readStream) nextImage
	