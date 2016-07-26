writing
writeOn: stream
	| writeStream |
	writeStream := ZnBivalentWriteStream on: stream.
	writeStream nextPutAll: self method; space.
	(ZnNetworkingUtils shouldProxyUrl: self uri)
		ifTrue: [ 
			self uri hasSecureScheme 
				ifTrue: [
					"See also ZnClient>>#setupTLSTo:"
					method = #CONNECT
						ifTrue: [ writeStream nextPutAll: uri authorityWithPort ]
						ifFalse: [ self uri printPathQueryFragmentOn: writeStream ] ]
				ifFalse: [ self uri printOn: writeStream ] ]
		ifFalse: [ self uri printPathQueryFragmentOn: writeStream ].
	writeStream space; nextPutAll: self version; nextPutAll: String crlf 