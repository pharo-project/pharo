multipart
parseParts
	"private -- parse the parts of the message and store them into a collection"
	"If this is not multipart, store an empty collection"
	| parseStream msgStream messages separator |
	self body isMultipart ifFalse: 
		[ parts := #().
		^ self ].

	"If we can't find a valid separator, handle it as if the message is not multipart"
	separator := self attachmentSeparator.
	separator ifNil: 
		[ Transcript
			show: 'Ignoring bad attachment separater';
			cr.
		parts := #().
		^ self ].
	separator := '--' , separator trimRight.
	parseStream := self bodyText readStream.
	msgStream := LimitingLineStreamWrapper 
		on: parseStream
		delimiter: separator.
	msgStream limitingBlock: 
		[ :aLine | 
		aLine trimRight = separator or: 
			[ "Match the separator"
			aLine trimRight = (separator , '--') ] ].	"or the final separator with --"

	"Throw away everything up to and including the first separator"
	msgStream upToEnd.
	msgStream skipThisLine.

	"Extract each of the multi-parts as strings"
	messages := OrderedCollection new.
	[ parseStream atEnd ] whileFalse: 
		[ messages add: msgStream upToEnd.
		msgStream skipThisLine ].
	parts := messages collect: [ :e | MailMessage from: e ]