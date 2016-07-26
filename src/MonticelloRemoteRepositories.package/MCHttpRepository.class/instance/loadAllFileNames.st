i/o
loadAllFileNames
	| client |
	self displayProgress: 'Loading all file names from ', self description during: [
		client := self httpClient.
		client
			ifFail: [ :exception | 
				(exception className beginsWith: 'Zn')
					ifTrue: [ MCRepositoryError signal: 'Could not access ', self location, ': ', exception printString ]
					ifFalse: [ exception pass ] ];
			url: self locationWithTrailingSlash;
			queryAt: 'C' put: 'M;O=D';	"legacy that some servers maybe expect"
			get.
		self assertNonBinaryResponse: client response ].
	^ self parseFileNamesFromStream: client contents readStream