Extension { #name : #MIMEDocument }

{ #category : #'*Network-Mail' }
MIMEDocument >> parts [
	"Return the parts of this message.  There is a far more reliable implementation of parts in MailMessage, but for now we are continuing to use this implementation"
	| parseStream currLine separator msgStream messages |
	self isMultipart ifFalse: 
		[ ^ #() ].
	parseStream := self content readStream.
	currLine := ''.
	[ '--*' match: currLine ] whileFalse: [ currLine := parseStream nextLine ].
	separator := currLine copy.
	msgStream := LimitingLineStreamWrapper 
		on: parseStream
		delimiter: separator.
	messages := OrderedCollection new.
	[ parseStream atEnd ] whileFalse: 
		[ messages add: msgStream upToEnd.
		msgStream skipThisLine ].
	^ messages collect: [ :e | MailMessage from: e ]
]
