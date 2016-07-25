testing
testUrl
	self withServerDo: [ :server | 
		self assert: server url equals: server localUrl.
		server serverUrl:
					(ZnUrl new
						host: 'zn.stfx.eu';
						yourself).
		self assert: server url scheme equals: #http.
		self assert: server url host equals: 'zn.stfx.eu'.
		self assert: server url port equals: server port.
		server serverUrl: 'http://zn.stfx.eu:8080/zn'.
		self assert: server url scheme equals: #http.
		self assert: server url host equals: 'zn.stfx.eu'.
		self assert: server url port equals: 8080.
		self assert: server url path equals: 'zn' ]