testing
assertEntityTooLarge: serverUrl
	| response client |
	client := ZnClient new
		url: serverUrl;
		addPathSegment: #echo;
		entity: (ZnEntity with: (ByteArray new: self entitySizeLimit + 1));
		yourself.
	response := client
		post;
		response.
	client close.
	self deny: response isSuccess.
	self assert: response code equals: 400.
	self assert: response contentType equals: ZnMimeType textPlain.
	self assert: (response entity contents includesSubstring: 'ZnEntityTooLarge')