tests
testBasicGetWide
	self withServerDo: [ :server | | client |
		(client := ZnClient new) 
			beOneShot;
			url: server localUrl;
			addPath: #('local-files' 'wide.html');
			get.
		self assert: client isSuccess.
		self assert: client response contentType = ZnMimeType textHtml.
		self assert: client contents equals: self wideHtml.
		self 
			assert: (ZnUtils parseHttpDate: (client response headers at: 'Modification-Date'))
			equals: (ZnFileSystemUtils modificationTimeFor: 'large.html') asUTC.
		self 
			assert: (ZnUtils parseHttpDate: (client response headers at: 'Expires')) > (DateAndTime now + 10 days).
		self 
			assert: (client response headers at: 'Cache-Control')
			equals: (server delegate maxAgeFor: ZnMimeType textHtml) ]