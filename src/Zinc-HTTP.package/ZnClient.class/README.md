I am ZnClient, an object to build, execute and process HTTP client requests.

I have a rich protocol to construct requests and to access responses.
I have various error handling options.
I can reuse an existing connection to a specific host:port.
I handle sessions, cookies, redirects and authentication.
I have many options (settings) with sensible defaults.

Simplest possible invocation:

	ZnClient new
		get: 'http://zn.stfx.eu/zn/numbers.txt'.
	
Using some features to make a better HTTP request:

	ZnClient new
		systemPolicy;
		accept: ZnMimeType textPlain;
		http;
		host: 'zn.stfx.eu';
		path: 'zn/numbers.txt';
		contentReader: [ :entity | entity contents lines collect: [ :each | each asNumber ] ];
		ifFail: [ :exception | self inform: 'I am sorry: ', exception printString ];
		get.

Part of Zinc HTTP Components.