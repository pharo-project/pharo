testing
testUnspecifiedEncoding
	| json bytes entity |
	json := '{"text":"Der Weg zur Hölle ist mit guten Vorsätzen gepflastert."}'.
	entity := ZnEntity with: json type: ZnMimeType applicationJson setCharSetUTF8.
	bytes := entity asByteArray.
	ZnDefaultCharacterEncoder 
		value: ZnUTF8Encoder new
		during: [
			entity := ZnEntity 
				readFrom: bytes readStream 
				usingType: ZnMimeType applicationJson 
				andLength: nil. 
			self assert: entity contents equals: json.
			entity := ZnEntity with: json type: ZnMimeType applicationJson.
			self assert: entity encoder equals: ZnUTF8Encoder new ]