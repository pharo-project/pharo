initialize-release
postRequest
	message := (ZnRequest post: 'http://zn.stfx.eu/echo/one/two/three?param1=123&param2=foobar')
		setAcceptEncodingGzip;
		setBasicAuthenticationUsername: 'john' password: 'secret';
		entity: (ZnEntity with: (self randomBytes: 512));
		yourself