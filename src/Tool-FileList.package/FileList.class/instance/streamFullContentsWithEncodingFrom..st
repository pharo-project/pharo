private
streamFullContentsWithEncodingFrom: aBinaryReadStream
	^ (ZnCharacterReadStream on: aBinaryReadStream encoding: self getEncoding) upToEnd