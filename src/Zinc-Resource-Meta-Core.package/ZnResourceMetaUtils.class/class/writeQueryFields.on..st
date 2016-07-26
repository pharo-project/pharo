conversions
writeQueryFields: dictionary on: stream
	self writeQueryFields: dictionary withEncoder: ZnUTF8Encoder new on: stream 