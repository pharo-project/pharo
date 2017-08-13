testing
testWhitespaceAtEnd
	| encoder |
	encoder := ZnBase64Encoder new.
	"whitespace is #any non-alphabet character"
	self assert: (encoder decode: 'TQ==' , String lf) equals: 'M' asByteArray.
	encoder whitespace: #separator.
	self assert: (encoder decode: 'TQ==' , String lf) equals: 'M' asByteArray.
	encoder whitespace: nil.
	self should: [ encoder decode: 'TQ==' , String lf ] raise: ZnCharacterEncodingError 