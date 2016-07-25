testing
isGzipped
	| contentEncoding |
	contentEncoding := self headers at: 'Content-Encoding' ifAbsent: [ ^ false ].
	^ contentEncoding = 'gzip'