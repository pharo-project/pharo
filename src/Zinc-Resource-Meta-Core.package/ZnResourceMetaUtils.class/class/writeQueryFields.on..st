conversions
writeQueryFields: dictionary on: stream
	self writeQueryFields: dictionary withEncoder: ZnCharacterEncoder utf8 on: stream 