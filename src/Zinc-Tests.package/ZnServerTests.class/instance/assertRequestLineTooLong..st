testing
assertRequestLineTooLong: serverUrl
	| url response |
	url := serverUrl
		addPathSegment: #echo;
		addPathSegment: (String new: ZnConstants maximumLineLength withAll: $X);
		yourself.
	response := ZnEasy get: url.
	self deny: response isSuccess.
	self assert: response code equals: 400.
	self assert: response contentType equals: ZnMimeType textPlain.
	self assert: (response entity contents includesSubstring: 'ZnLineTooLong')