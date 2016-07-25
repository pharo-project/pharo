testing
acceptsEncodingGzip
	| value |
	value := self headers at: 'Accept-Encoding' ifAbsent: [ ^ false ].
	^ value includesSubstring: 'gzip'