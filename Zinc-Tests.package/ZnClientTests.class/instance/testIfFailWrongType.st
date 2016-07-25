testing
testIfFailWrongType
	| client |
	(client := ZnClient new)
		beOneShot;
		accept: ZnMimeType imagePng;
		enforceAcceptContentType: true;
		url: self smallHtmlUrl;
		ifFail: [ :exception |
			^ self assert: (exception isKindOf: ZnUnexpectedContentType) ];
		get.
	self fail