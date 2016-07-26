testing
isConnectionClose
	| value |
	value := self headers at: 'Connection' ifAbsent: [ ^ false ].
	^ value sameAs: 'close'