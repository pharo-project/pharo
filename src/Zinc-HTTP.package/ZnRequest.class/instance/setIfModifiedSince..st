accessing
setIfModifiedSince: reference
	self headers at: 'If-Modified-Since' put: (ZnUtils httpDate: reference)