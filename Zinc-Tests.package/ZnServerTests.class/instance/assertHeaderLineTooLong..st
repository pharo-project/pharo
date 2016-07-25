testing
assertHeaderLineTooLong: serverUrl
	| response client |
	client := ZnClient new
		url: serverUrl;
		addPathSegment: #echo;
		headerAt: 'X-Test' put: (String new: ZnConstants maximumLineLength withAll: $X);
		yourself.
	response := client
		get;
		response.
	client close.
	self deny: response isSuccess.
	self assert: response code equals: 400.
	self assert: response contentType equals: ZnMimeType textPlain.
	self assert: (response entity contents includesSubstring: 'ZnLineTooLong')