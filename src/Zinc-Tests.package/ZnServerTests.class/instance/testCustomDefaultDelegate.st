testing
testCustomDefaultDelegate
	self withServerDo: [ :server | | customDelegate client |
		customDelegate := ZnDefaultServerDelegate empty.
		customDelegate 
			map: 'ok' to: [ :request | ZnResponse ok: (ZnEntity text: 'OK') ];
			map: '/' to: 'ok';
			map: 'OK' to: [ :request | ZnResponse redirect: '/ok' ].
		server delegate: customDelegate.
		client := ZnClient new url: server localUrl.
		self assert: client get equals: 'OK'.
		client addPath: 'OK'.
		self assert: client get equals: 'OK'.
		client close	 ]