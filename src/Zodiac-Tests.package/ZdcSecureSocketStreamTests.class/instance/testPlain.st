tests
testPlain
	| query stream request response |
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	query := 'Smalltalk'.
	stream := ZdcSecureSocketStream openConnectionToHostNamed: 'encrypted.google.com' port: 443.
	[
		request := String streamContents: [ :out |
			out << 'GET /search?q=' << query << ' HTTP/1.1' << String crlf.
			out << 'Host: encrypted.google.com' << String crlf.
			out << 'Connection: close' << String crlf.
			out << String crlf ].
		stream connect.
		stream nextPutAll: request asByteArray.
		stream flush.
		response := stream upToEnd asString.
	] ensure: [ stream close ].
	self assert: (response includesSubstring: '200 OK').      
	self assert: (response includesSubstring: 'Google').
	self assert: (response includesSubstring: 'Smalltalk').      