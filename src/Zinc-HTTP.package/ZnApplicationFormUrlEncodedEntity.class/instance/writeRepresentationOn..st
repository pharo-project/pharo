writing
writeRepresentationOn: stream
	self contentType charSet
		ifNotNil: [:charSet|
			ZnResourceMetaUtils
				writeQueryFields: self fields 
				withEncoder: (ZnCharacterEncoder newForEncoding: charSet)
				on: stream] 
		ifNil: [ 
			ZnResourceMetaUtils 
				writeQueryFields: self fields 
				on: stream ]