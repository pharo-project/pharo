private
httpClient
	"Return a new, specifically configured instance of the HTTP client for internal use.
	Note how we request GZIP compression and will signal progress."

	^ ZnClient new
		systemPolicy;
		beOneShot;
		username: self user password: self password;
	
		signalProgress: true;
		yourself