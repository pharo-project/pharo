instance creation
statusCode: httpResponeCode
	^ self statusLine: (ZnStatusLine code: httpResponeCode)