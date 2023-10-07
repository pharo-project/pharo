Extension { #name : 'MIMEDocument' }

{ #category : '*Network-Mail' }
MIMEDocument >> parts [
	"Return the parts of this message.  There is a far more reliable implementation of parts in MailMessage, but for now we are continuing to use this implementation"
	| parseStream currLine separator msgStream messages currentLine |
	self isMultipart ifFalse:
		[ ^ #() ].
	parseStream := self content readStream.
	currLine := ''.
	[ '--*' match: currLine ] whileFalse: [ currLine := parseStream nextLine ].
	separator := currLine copy.

	msgStream := ZnFastLineReader on: parseStream.
	messages := OrderedCollection new.
	currentLine := ''.
	[ parseStream atEnd or: [ currentLine beginsWith: separator ] ] whileFalse: [
		| message |
		message := String streamContents: [:strm |
			[(currentLine := msgStream nextLine) isNil] whileFalse: [
				strm nextPutAll: currentLine; cr]].
		messages add: message.
		currentLine := msgStream nextLine ].
	^ messages collect: [ :e | MailMessage from: e ]
]
