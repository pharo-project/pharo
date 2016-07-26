testing
isConnectionKeepAlive
	| value |
	value := self headers at: 'Connection' ifAbsent: [ ^ false ].
	^ value sameAs: 'keep-alive'