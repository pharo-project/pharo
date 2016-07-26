private
computeContentLength
	self string isNil ifTrue: [ ^ self ].
	contentLength := self encoder encodedByteCountForString: string