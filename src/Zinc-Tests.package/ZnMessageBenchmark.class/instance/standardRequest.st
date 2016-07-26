initialize-release
standardRequest
	message := (ZnRequest get: 'http://zn.stfx.eu/echo/one/two/three?param1=123&param2=foobar')
		setAcceptEncodingGzip;
		setBasicAuthenticationUsername: 'john' password: 'secret';
		yourself