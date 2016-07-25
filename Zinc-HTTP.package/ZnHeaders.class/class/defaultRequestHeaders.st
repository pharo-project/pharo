instance creation
defaultRequestHeaders
	^ self new
		at: 'User-Agent' put: ZnConstants defaultUserAgent;
		at: 'Accept' put: '*/*';
		yourself