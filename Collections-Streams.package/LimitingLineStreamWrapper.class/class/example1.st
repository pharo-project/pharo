examples
example1
	"LimitingLineStreamWrapper example1"
	"Separate chunks of text delimited by a special string"
	| inStream msgStream messages |
	inStream := self exampleStream.
	msgStream := self on: inStream delimiter: 'From '.
	messages := OrderedCollection new.
	[inStream atEnd] whileFalse: [
		msgStream skipThisLine.
		messages add: msgStream upToEnd].
	^messages
			