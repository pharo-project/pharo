instance creation
defaultResponseHeaders
	^ self new
		at: 'Server' put: ZnConstants defaultServerString;
		at: 'Date' put: ZnUtils httpDate;
		yourself