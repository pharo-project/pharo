testing
testIfFailNonExistingHost
	| client |
	"Note that when there is an HTTP proxy set, there will be no direct Error"
	(client := ZnClient new)
		beOneShot;
		url: 'http://unknown-stfx.eu';
		ifFail: [ : exception |
			^ self assert: (exception isKindOf: NetworkError) | (exception isKindOf: ZnHttpUnsuccessful) ];
		get.
	self assert: client isSuccess not.
	client
		enforceHttpSuccess: true;
		get.
	self fail