accessing
setTransferEncodingChunked
	self headers 
		at: 'Transfer-Encoding' put: 'chunked';
		clearContentLength