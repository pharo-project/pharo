private
streamFirst5000BytesWithEncodingFrom: aBinaryReadStream
	^ (ZnCharacterReadStream on: aBinaryReadStream encoding: self getEncoding) next: 5000