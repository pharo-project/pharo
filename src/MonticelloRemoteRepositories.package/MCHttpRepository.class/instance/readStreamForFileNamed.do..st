i/o
readStreamForFileNamed: aString do: aBlock
	| client |
	self displayProgress: 'Downloading ', aString during: [
		client := self httpClient.
		client
			ifFail: [ :exception | self error: 'Could not load ', aString, ': ', exception printString ];
			get: (self urlForFileNamed: aString).
		self assertBinaryResponse: client response.
		"immediately cache the version and avoid an unnecessary serialization"
		self cacheRawVersionNamed: aString stream: client contents ].
	^ aBlock value: client contents readStream