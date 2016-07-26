instance creation
readFrom: textStringOrStream
	"Create an object based on the contents of textStringOrStream."

	| object |
	object := self class compiler evaluate: textStringOrStream.
	(object isKindOf: self) ifFalse: [self error: self name, ' expected'].
	^object