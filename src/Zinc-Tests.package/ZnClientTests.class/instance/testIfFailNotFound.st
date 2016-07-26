testing
testIfFailNotFound
	| client |
	(client := ZnClient new)
		beOneShot;
		url: 'http://stfx.eu/non-existing';
		ifFail: [ :exception |
			^ self assert: (exception isKindOf: ZnHttpUnsuccessful) ];
		get.
	self assert: client isSuccess not.
	client
		enforceHttpSuccess: true;
		get.
	self fail