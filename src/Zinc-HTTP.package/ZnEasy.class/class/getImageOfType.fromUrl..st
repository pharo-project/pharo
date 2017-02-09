private
getImageOfType: mimeType fromUrl: urlObject
	| client |
	(client := self client)
		url: urlObject;
		accept: mimeType;
		enforceHttpSuccess: true;
		enforceAcceptContentType: true;
		get.
	"ImageReadWriter does automatic type detection"
	^ ImageReadWriter formFromStream: client entity readStream