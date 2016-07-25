interface
loadAllFileNames
	| client |
	client := self httpClient.
	client
		ifFail: [ :exception | self error: 'Could not access ', self location, ': ', exception printString ];
		url: self locationWithTrailingSlash;
		queryAt: 'format' put: 'raw';
		get.
	self assertNonBinaryResponse: client response.
	^ self parseFileNamesFromStream: client contents