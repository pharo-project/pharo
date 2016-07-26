instance creation
code: code
	^ self new
		code: code;
		version: ZnConstants defaultHTTPVersion;
		yourself