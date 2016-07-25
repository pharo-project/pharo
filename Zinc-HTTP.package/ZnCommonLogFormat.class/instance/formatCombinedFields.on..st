private
formatCombinedFields: request on: stream
	| referer userAgent |
	referer := request headers at: 'Referer' ifAbsent: [ '-' ].
	userAgent := request headers at: 'User-Agent' ifAbsent: [ '-' ].
	stream space.
	stream nextPut: $"; << referer; nextPut: $".
	stream space.
	stream nextPut: $"; << userAgent; nextPut: $"
