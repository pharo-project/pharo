testing
testRespond
	self withServerDo: [ :server | | client |
		server onRequestRespond: [ :request |
			ZnRespond signalWith: (ZnResponse ok: (ZnEntity text: 'RESPONDED')).
			ZnResponse notFound: request uri ].
		client := ZnClient new.
		client get: server localUrl.
		self assert: client isSuccess.
		self assert: client response contentType equals: ZnMimeType textPlain.
		self assert: client contents equals: 'RESPONDED'.
		client close ]