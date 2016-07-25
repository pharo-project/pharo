testing
isChunked
	| transferEncoding |
	transferEncoding := self headers at: 'Transfer-Encoding' ifAbsent: [ ^ false ].
	^ transferEncoding = 'chunked' or: [ transferEncoding beginsWith: 'chunked' ]