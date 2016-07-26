testing
testGzipCompressionAndChunking
	self withServerDo: [ :server | | client |
		server useGzipCompressionAndChunking: true.
		(client := ZnClient new)
			url: server localUrl;
			addPathSegment: 'dw-bench';
			setAcceptEncodingGzip;
			get.
		self assert: client isSuccess.
		self assert: client response hasContentEncoding.
		self assert: (client response headers at: 'Content-Encoding') equals: 'gzip'.
		self assert: client response hasTransferEncoding.
		self assert: (client response headers at: 'Transfer-Encoding') equals: 'chunked' ]