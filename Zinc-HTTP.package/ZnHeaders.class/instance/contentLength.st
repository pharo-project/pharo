accessing
contentLength
	"We allow multiple content-length headers provided they are identical."
	
	| value |
	(value := self headers at: 'Content-Length') isString
		ifFalse: [
			value asSet size = 1
				ifTrue: [ value := value first ]
				ifFalse: [ self error: 'Multiple, different Content-Length headers are not allowed' ] ].
	^ Integer readFrom: value ifFail: [ self error: 'Illegal HTTP Content Length' ] 