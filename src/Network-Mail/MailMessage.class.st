"
I represent an Internet mail or news message.

	text - the raw text of my message
	body - the body of my message, as a MIMEDocument
	fields - a dictionary mapping lowercased field names into collections of MIMEHeaderValue's
	parts - if I am a multipart message, then this is a cache of my parts
"
Class {
	#name : #MailMessage,
	#superclass : #Object,
	#instVars : [
		'text',
		'body',
		'fields',
		'parts'
	],
	#category : #'Network-Mail'
}

{ #category : #utilities }
MailMessage class >> dateStampNow [
	"Return the current date and time formatted per RFC5322  e.g.  'Thu, 18 Feb 1999 20:38:51 -0500'"

	^	DateAndTime now asEmailString.
]

{ #category : #utilities }
MailMessage class >> dateTimeSuffix [
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc."

	^self monthDayTime24StringFrom: Time primUTCSecondsClock
]

{ #category : #'instance creation' }
MailMessage class >> empty [
	"return a message with no text and no header"

	^self new
]

{ #category : #'instance creation' }
MailMessage class >> from: aString [
	"Initialize a new instance from the given string."

	^ self new from: aString
]

{ #category : #'instance creation' }
MailMessage class >> from: senderString to: recipients about: subjectString asFollows: bodyString [ 

	^ (self fromRfc822: Character lf asString, bodyString)
			from: senderString;
			date: self dateStampNow;
			subject: subjectString;
			to: recipients;
			yourself.
]

{ #category : #'instance creation' }
MailMessage class >> fromRfc822: aString [
	"Initialize a new instance from the given string."

	^ self new fromRfc822: aString
]

{ #category : #utilities }
MailMessage class >> generateSeparator [
	"generate a separator usable for making MIME multipart documents.  A leading -- will *not* be included"
	^'==CelesteAttachment' , (10000 to: 99999) atRandom asString , '=='.
]

{ #category : #utilities }
MailMessage class >> monthDayTime24StringFrom: aSecondCount [
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the compact form
             ddMMMhhmm		where dd is a two-digit day-of-month, MMM is the alpha month abbreviation and hhmm is the time on a 24-hr clock.

          Utilities monthDayTime24StringFrom: Time primSecondsClock
"

	aDate := Date fromSeconds: aSecondCount.
	aTime := Time fromSeconds: aSecondCount \\ 86400.

	^ (aDate dayOfMonth asTwoCharacterString), 
		(aDate monthName copyFrom: 1 to: 3), 
		(aTime hhmm24)
]

{ #category : #preferences }
MailMessage class >> omittedHeaderFields [
	"Reply a list of fields to omit when displaying a nice simple message"

	"Note that heads of the form
		X-something: value
	are filtered programatically.  This is done since we don't want any of them
	and it is impossible to predict them in advance."

	^ #(
			'comments'
			'priority'
			'disposition-notification-to'
			'content-id'
			'received'
			'return-path'
			'newsgroups'
			'message-id'
			'path'
			'in-reply-to'
			'sender'
			'fonts'
			'mime-version'
			'status'
			'content-type'
			'content-transfer-encoding'
			'errors-to'
			'keywords'
			'references'
			'nntp-posting-host'
			'lines'
			'return-receipt-to'
			'precedence'
			'originator'
			'distribution'
			'content-disposition'
			'importance'
			'resent-to'
			'resent-cc'
			'resent-message-id'
			'resent-date'
			'resent-sender'
			'resent-from'
			'delivered-to'
			'user-agent'
			'content-class'
			'thread-topic'
			'thread-index'
			'list-help'
			'list-post'
			'list-subscribe'
			'list-id'
			'list-unsubscribe'
			'list-archive'
		)

]

{ #category : #multipart }
MailMessage >> addAlternativePart: newPart [
	self makeMultipart: 'alternative' with: newPart.

]

{ #category : #multipart }
MailMessage >> addAlternativePart: bodyString contentType: aContentTypeString [

	| newPart |
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: aContentTypeString.
	newPart body: (MIMEDocument contentType: aContentTypeString  content: bodyString).
	self addAlternativePart: newPart.

]

{ #category : #multipart }
MailMessage >> addAttachmentFrom: aStream withName: aName [
	"add an attachment, encoding with base64.  aName is the option filename to encode"
	| newPart |
	self makeMultipart.
	self parts.  "make sure parts have been parsed"

	"create the attachment as a MailMessage"
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: 'application/octet-stream'.
	newPart setField: 'content-transfer-encoding' toString: 'base64'.
	aName ifNotNil: [
		| dispositionField |
		dispositionField := MIMEHeaderValue fromMIMEHeader: 'attachment'.
		dispositionField parameterAt: 'filename' put: aName.
		newPart setField: 'content-disposition' to: dispositionField ].
	newPart body: (MIMEDocument contentType: 'application/octet-stream' content: aStream upToEnd).


	"regenerate our text"
	parts := parts copyWith: newPart.
	self regenerateBodyFromParts.
	text := nil.
]

{ #category : #multipart }
MailMessage >> addMixedPart: newPart [
	self makeMultipart: 'mixed' with: newPart.

]

{ #category : #multipart }
MailMessage >> addMixedPart: bodyString contentType: aContentTypeString [

	| newPart |
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: aContentTypeString.
	newPart body: (MIMEDocument contentType: aContentTypeString  content: bodyString).
	self addMixedPart: newPart.

]

{ #category : #'printing/formatting' }
MailMessage >> asSendableText [
	"break lines in the given string into shorter lines"
	| result atAttachment width aString pastHeader |
	width := 72.
	aString := self text.
	result := (String new: aString size * 50 // 49) writeStream.
	pastHeader := false.
	atAttachment := false.
	aString asString
		linesDo: 
			[:line | | end start | 
			line isEmpty ifTrue: [pastHeader := true].
			pastHeader
				ifTrue: 
					[(line beginsWith: '--==')
						ifTrue: [atAttachment := true].
					atAttachment
						ifTrue: 
							["at or after an attachment line; no more 
							wrapping for the rest of the message"
							result nextPutAll: line.
							result cr]
						ifFalse: [(line beginsWith: '>')
								ifTrue: 
									["it's quoted text; don't wrap it"
									result nextPutAll: line.
									result cr]
								ifFalse: 
									["regular old line.  Wrap it to multiple 
									lines "
									start := 1.
									"output one shorter line each time 
									through this loop"
									[start + width <= line size]
										whileTrue: 
											["find the end of the line"
											end := start + width - 1.
											[end >= start and: [(line at: end + 1) isSeparator not]]
												whileTrue: [end := end - 1].
											end < start ifTrue: ["a word spans the entire 
												width! "
												end := start + width - 1].
											"copy the line to the output"
											result nextPutAll: (line copyFrom: start to: end).
											result cr.
											"get ready for next iteration"
											start := end + 1.
											(line at: start) isSeparator ifTrue: [start := start + 1]].
									"write out the final part of the line"
									result nextPutAll: (line copyFrom: start to: line size).
									result cr]]]
				ifFalse: 
					[result nextPutAll: line.
					result cr]].
	^ result contents
]

{ #category : #multipart }
MailMessage >> atomicParts [
	"Answer all of the leaf parts of this message, including those of multipart included messages"

	self body isMultipart ifFalse: [^ OrderedCollection with: self].
	^ self parts inject: OrderedCollection new into: [:col :part | col , part atomicParts]
]

{ #category : #multipart }
MailMessage >> attachmentSeparator [
	^(self fieldNamed: 'content-type' ifAbsent: [^nil]) parameters
		at: 'boundary' ifAbsent: [^nil]
]

{ #category : #accessing }
MailMessage >> body [
	"return just the body of the message"
	^body
]

{ #category : #initialization }
MailMessage >> body: newBody [
	"change the body"
	body := newBody.
	text := nil.
]

{ #category : #accessing }
MailMessage >> bodyText [
	"return the text of the body of the message"
	^body content
]

{ #category : #'printing/formatting' }
MailMessage >> bodyTextFormatted [
	"Answer a version of the text in my body suitable for display.  This will parse multipart forms, decode HTML, and other such things"

	"check for multipart"

	self body isMultipart
		ifTrue: [ 
			"check for alternative forms"
			self body isMultipartAlternative
				ifTrue: [ 
					"it's multipart/alternative.  search for a part that we can display, biasing towards nicer formats"
					#('text/html' 'text/plain')
						do: [ :format | 
							self parts
								do: [ :part | 
									part body contentType = format
										ifTrue: [ ^ part bodyTextFormatted ] ] ].	"couldn't find a desirable part to display; just display the first part"
					^ self parts first bodyTextFormatted ].	"not alternative parts.  put something for each part"
			^ Text
				streamContents: [ :str | 
					self parts
						do: [ :part | 
							((#('text' 'multipart') includes: part body mainType) or: [ part body contentType = 'message/rfc822' ])
								ifTrue: [ 
									"try to inline the message part"
									str nextPutAll: part bodyTextFormatted ]
								ifFalse: [ 
									| descript |
									str cr.
									descript := part name ifNil: [ 'attachment' ].
									str nextPutAll: (Text string: '[' , descript , ']' attribute: (TextMessageLink message: part)) ] ] ] ].	"check for HTML"
	self body contentType = 'text/html'
		ifTrue: [ 
			Smalltalk globals
				at: #HtmlParser
				ifPresent: [ :htmlParser | ^ (htmlParser parse: body content readStream) formattedText ] ].	"check for an embedded message"
	self body contentType = 'message/rfc822'
		ifTrue: [ ^ (MailMessage from: self body content) formattedText ].	"nothing special--just return the text"
	^ body content
]

{ #category : #accessing }
MailMessage >> cc [

	^self fieldsNamed: 'cc' separatedBy: ', '
]

{ #category : #'printing/formatting' }
MailMessage >> cleanedHeader [
	"Reply with a cleaned up version email header.  First show fields people would normally want to see (in a regular order for easy browsing), and then any other fields not explictly excluded"

	| new priorityFields omittedFields |
	new := (String new: text size) writeStream.
	priorityFields := #('Date' 'From' 'Subject' 'To' 'Cc').
	omittedFields := MailMessage omittedHeaderFields.	"Show the priority fields first, in the order given in priorityFields"
	priorityFields
		do: [ :pField | 
			"We don't check whether the priority field is in the omitted list!"
			self
				headerFieldsNamed: pField
				do: [ :fValue | 
					new
						nextPutAll: pField;
						nextPutAll: ': ';
						nextPutAll: fValue decodeMimeHeader;
						cr ] ].	"Show the rest of the fields, omitting the uninteresting ones and ones we have already shown"
	omittedFields := omittedFields , priorityFields.
	self
		fieldsFrom: text readStream
		do: [ :fName :fValue | 
			((fName beginsWith: 'x-') or: [ omittedFields anySatisfy: [ :omitted | fName sameAs: omitted ] ])
				ifFalse: [ 
					new
						nextPutAll: fName;
						nextPutAll: ': ';
						nextPutAll: fValue;
						cr ] ].
	^ new contents
]

{ #category : #testing }
MailMessage >> containsViewableImage [
	^self body isJpeg | self body isGif | self body isPng
]

{ #category : #accessing }
MailMessage >> date [
	"Answer a date string for this message."

	^(Date fromSeconds: self time + (Date year: 1980 day: 1 ) asSeconds) 
		printFormat: #(2 1 3 47 1 2)
]

{ #category : #'fields-convenience' }
MailMessage >> date: aDate [
	self setField: 'date' toString: aDate
]

{ #category : #multipart }
MailMessage >> decoderClass [
	| encoding |
	encoding := self fieldNamed: 'content-transfer-encoding' ifAbsent: [^ nil].
	encoding := encoding mainValue.
	encoding asLowercase = 'base64' ifTrue: [^ Base64MimeConverter].
	encoding asLowercase = 'quoted-printable' ifTrue: [^ QuotedPrintableMimeConverter].
	^ nil
]

{ #category : #'printing/formatting' }
MailMessage >> excerpt [
	"Return a short excerpt of the text of the message"

	^ self bodyText withSeparatorsCompacted truncateWithElipsisTo: 60
]

{ #category : #fields }
MailMessage >> fieldNamed: aString ifAbsent: aBlock [
	| matchingFields |
	"return the value of the field with the specified name.  If there is more than one field, then return the first one"
	matchingFields := fields at: aString asLowercase ifAbsent: [ ^aBlock value ].
	^matchingFields first
]

{ #category : #accessing }
MailMessage >> fields [
	"return the internal fields structure.  This is private and subject to change!"
	^ fields
]

{ #category : #parsing }
MailMessage >> fieldsFrom: aStream do: aBlock [ 
	"Invoke the given block with each of the header fields from the given stream. The block arguments are the field name and value. The streams position is left right after the empty line separating header and body."
	| savedLine line s |
	savedLine := self readStringLineFrom: aStream.
	[ aStream atEnd ] whileFalse: 
		[ line := savedLine.
		line isEmpty ifTrue: [ ^ self ].	"quit when we hit a blank line"
		
		[ savedLine := self readStringLineFrom: aStream.
		savedLine notEmpty and: [ savedLine first isSeparator ] ] whileTrue: 
			[ "lines starting with white space are continuation lines"
			s := savedLine readStream.
			s skipSeparators.
			line := line , ' ' , s upToEnd ].
		self 
			reportField: line trimBoth
			to: aBlock ].

	"process final header line of a body-less message"
	savedLine isEmpty ifFalse: 
		[ self 
			reportField: savedLine trimBoth
			to: aBlock ]
]

{ #category : #fields }
MailMessage >> fieldsNamed: aString ifAbsent: aBlock [
	"return a list of all fields with the given name"
	^fields at: aString asLowercase ifAbsent: aBlock
]

{ #category : #fields }
MailMessage >> fieldsNamed: aString  separatedBy: separationString [
	"return all fields with the specified name, concatenated together with separationString between each element.  Return an empty string if no fields with the specified name are present"
	| matchingFields |
	matchingFields := self fieldsNamed: aString ifAbsent: [ ^'' ].
	^String streamContents: [ :str |
		matchingFields
			do: [ :field | str nextPutAll: field mainValue ]
			separatedBy: [ str nextPutAll: separationString ]].

]

{ #category : #'printing/formatting' }
MailMessage >> format [
	"Replace the text of this message with a formatted version."
	"NOTE: This operation discards extra header fields."

	text := self formattedText.
]

{ #category : #'printing/formatting' }
MailMessage >> formattedText [
	"Answer a version of my text suitable for display.  This cleans up the header, decodes HTML, and things like that"

	
	^ self cleanedHeader asText, String cr , self bodyTextFormatted
]

{ #category : #accessing }
MailMessage >> from [

	^(self fieldNamed: 'from' ifAbsent: [ ^'' ]) mainValue
]

{ #category : #'fields-convenience' }
MailMessage >> from: senderString [
	self setField: 'from' toString: senderString
]

{ #category : #initialization }
MailMessage >> fromRfc822: aString [ 
	"Parse aString to initialize myself."
	| parseStream contentType bodyText contentTransferEncoding |
	text := aString trimRight, String cr.
	parseStream := text readStream.
	contentType := 'text/plain'.
	contentTransferEncoding := nil.
	fields := Dictionary new.

	"Extract information out of the header fields"
	self 
		fieldsFrom: parseStream
		do: 
			[ :fName :fValue | 
			"NB: fName is all lowercase"
			fName = 'content-type' ifTrue: [ contentType := (fValue copyUpTo: $;) asLowercase ].
			fName = 'content-transfer-encoding' ifTrue: [ contentTransferEncoding := fValue asLowercase ].
			(fields 
				at: fName
				ifAbsentPut: [ OrderedCollection new: 1 ]) add: (MIMEHeaderValue 
					forField: fName
					fromString: fValue) ].

	"Extract the body of the message"
	bodyText := parseStream upToEnd.
	contentTransferEncoding = 'base64' ifTrue: 
		[ bodyText := Base64MimeConverter mimeDecodeToChars: bodyText readStream.
		bodyText := bodyText contents ].
	contentTransferEncoding = 'quoted-printable' ifTrue: [ bodyText := bodyText decodeQuotedPrintable ].
	body := MIMEDocument 
		contentType: contentType
		content: bodyText
]

{ #category : #fields }
MailMessage >> hasFieldNamed: aString [
	^fields includesKey: aString asLowercase
]

{ #category : #parsing }
MailMessage >> headerFieldsNamed: fieldName do: aBlock [ 
	"Evalue aBlock once for each header field which matches fieldName.  The block is valued with one parameter, the value of the field"
	self 
		fieldsFrom: text readStream
		do: [ :fName :fValue | (fieldName sameAs: fName) ifTrue: [ aBlock value: fValue ] ]
]

{ #category : #initialization }
MailMessage >> initialize [
	"initialize as an empty message"

	super initialize.
	text := String cr.
	fields := Dictionary new.
	body := MIMEDocument contentType: 'text/plain' content: String cr
]

{ #category : #multipart }
MailMessage >> makeMultipart [
	"if I am not multipart already, then become a multipart message with one part"

	| part multipartHeader |

	body isMultipart ifTrue: [ ^self ].

	"set up the new message part"
	part := MailMessage empty.
	part body: body.
	(self hasFieldNamed: 'content-type') ifTrue: [
		part setField: 'content-type' to: (self fieldNamed: 'content-type' ifAbsent: ['']) ].
	parts := Array with: part.

	"fix up our header"
	multipartHeader := MIMEHeaderValue fromMIMEHeader: 'multipart/mixed'.
	multipartHeader parameterAt: 'boundary' put: self class generateSeparator .
	self setField: 'content-type' to: multipartHeader.

	self setField: 'mime-version' to: (MIMEHeaderValue fromMIMEHeader: '1.0').
	self removeFieldNamed: 'content-transfer-encoding'.

	"regenerate everything"
	self regenerateBodyFromParts.
	text := nil.
]

{ #category : #multipart }
MailMessage >> makeMultipart: subType with: newPart [
	"if I am not multipart already, then become a multipart message with one part"

	| multipartHeader |

	body isMultipart
		ifFalse: [
			parts := Array with: newPart.

			"fix up our header"
			multipartHeader := MIMEHeaderValue fromMIMEHeader: 'multipart/',subType.
			multipartHeader parameterAt: 'boundary' put: self class generateSeparator .
			self setField: 'content-type' to: multipartHeader.

			self setField: 'mime-version' to: (MIMEHeaderValue fromMIMEHeader: '1.0').
			self removeFieldNamed: 'content-transfer-encoding']
		ifTrue: [
			self parts.
			parts := parts copyWith: newPart.
			].

	"regenerate everything"
	self regenerateBodyFromParts.
	text := nil.
]

{ #category : #accessing }
MailMessage >> name [
	"return a default name for this part, if any was specified.  If not, return nil"
	| type nameField disposition |

	"try in the content-type: header"
	type := self fieldNamed: 'content-type' ifAbsent: [nil].
	(type notNil and: [(nameField := type parameters at: 'name' ifAbsent: [nil]) notNil])
		ifTrue: [^ nameField].

	"try in content-disposition:"
	disposition := self fieldNamed: 'content-disposition' ifAbsent: [nil].
	(disposition notNil and: [(nameField := disposition parameters at: 'filename' ifAbsent: [nil]) notNil])
		ifTrue: [^ nameField].

	"give up"
	^ nil
]

{ #category : #multipart }
MailMessage >> parseParts [
	"private -- parse the parts of the message and store them into a collection"

	"If this is not multipart, store an empty collection"

	| parseStream msgStream messages separator |
	self body isMultipart
		ifFalse: [ parts := #().
			^ self ].

	"If we can't find a valid separator, handle it as if the message is not multipart"
	separator := self attachmentSeparator.
	separator
		ifNil: [ self traceCr: 'Ignoring bad attachment separater'.
			parts := #().
			^ self ].
	separator := '--' , separator trimRight.
	parseStream := self bodyText readStream.
	msgStream := LimitingLineStreamWrapper
		on: parseStream
		delimiter: separator.
	msgStream
		limitingBlock: [ :aLine | 
			aLine trimRight = separator
				or: [ "Match the separator" aLine trimRight = (separator , '--') ] ].	"or the final separator with --"

	"Throw away everything up to and including the first separator"
	msgStream upToEnd.
	msgStream skipThisLine.

	"Extract each of the multi-parts as strings"
	messages := OrderedCollection new.
	[ parseStream atEnd ]
		whileFalse: [ messages add: msgStream upToEnd.
			msgStream skipThisLine ].
	parts := messages collect: [ :e | MailMessage from: e ]
]

{ #category : #multipart }
MailMessage >> parts [
	parts ifNil: [self parseParts].
	^ parts
]

{ #category : #printing }
MailMessage >> printOn: aStream [ 
	"For text parts with no filename show: 'text/plain: first line of text...'    
	for attachments/filenamed parts show: 'attachment: filename.ext'"

	| name |

	aStream nextPutAll: ((name := self name) ifNil: ['Text: ' , self excerpt]
			ifNotNil: ['File: ' , name])
]

{ #category : #parsing }
MailMessage >> readDateFrom: aStream [
	"Parse a date from the given stream and answer nil if the date can't be parsed. The date may be in any of the following forms:
		<day> <monthName> <year>		(5 April 1982; 5-APR-82)
		<monthName> <day> <year>		(April 5, 1982)
		<monthNumber> <day> <year>		(4/5/82)
	In addition, the date may be preceded by the day of the week and an optional comma, such as:
		Tue, November 14, 1989"

	| day month year |
	self skipWeekdayName: aStream.
	aStream peek isDigit ifTrue: [day := Integer readFrom: aStream].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue:		"month name or weekday name"
			[month := (String new: 10) writeStream.
			 [aStream peek isLetter] whileTrue: [month nextPut: aStream next].
			 month := month contents.
			 day ifNil:		"name/number..."
				[[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
				 (aStream peek isDigit) ifFalse: [^nil].
				 day := Integer readFrom: aStream]]
		ifFalse:		"number/number..."
			[month := Date nameOfMonth: day.
			 day := Integer readFrom: aStream].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	(aStream peek isDigit) ifFalse: [^nil].
	year := Integer readFrom: aStream.
	^Date year: year month: month day: day
]

{ #category : #parsing }
MailMessage >> readStringLineFrom: aStream [ 
	"Read and answer the next line from the given stream. Consume the carriage return but do not append it to the string."

	^aStream nextLine
]

{ #category : #accessing }
MailMessage >> recipientList [

	^ (self to findTokens: $,) collect: [ :e | e trimLeft ]
]

{ #category : #'printing/formatting' }
MailMessage >> regenerateBodyFromParts [
	"regenerate the message body from the multiple parts"
	| bodyText |

	bodyText := String streamContents: [ :str |
		str cr.
		parts do: [ :part |
			str
				cr;
				nextPutAll: '--';
				nextPutAll: self attachmentSeparator;
				cr;
				nextPutAll: part text ].
	
		str
			cr;
			nextPutAll: '--';
			nextPutAll: self attachmentSeparator;
			nextPutAll: '--';
			cr ].

	body := MIMEDocument contentType: 'multipart/mixed' content: bodyText.
	text := nil.  "text needs to be reformatted"
]

{ #category : #'printing/formatting' }
MailMessage >> regenerateText [
	"regenerate the full text from the body and headers"
	
	text := String streamContents: 
		[ :str | | encodedBodyText | 
		"first put the header"
		fields keysAndValuesDo: 
			[ :fieldName :fieldValues | 
			fieldValues do: 
				[ :fieldValue | 
				str
					nextPutAll: fieldName capitalized;
					nextPutAll: ': ';
					nextPutAll: fieldValue asHeaderValue;
					cr ] ].

		"skip a line between header and body"
		str cr.

		"put the body, being sure to encode it according to the header"
		encodedBodyText := body content.
		self decoderClass ifNotNil: 
			[ encodedBodyText := self decoderClass mimeEncode: encodedBodyText readStream ].
		str nextPutAll: encodedBodyText contents]
]

{ #category : #fields }
MailMessage >> removeFieldNamed: name [
	"remove all fields with the specified name"
	fields removeKey: name ifAbsent: []
]

{ #category : #parsing }
MailMessage >> reportField: aString to: aBlock [ 
	"Evaluate the given block with the field name a value in the given field. Do nothing if the field is malformed."
	| s fieldName fieldValue |
	(aString includes: $:) ifFalse: [ ^ self ].
	s := aString readStream.
	fieldName := (s upTo: $:) asLowercase.	"fieldname must be lowercase"
	fieldValue := s upToEnd trimBoth.
	fieldValue isEmpty ifFalse: 
		[ aBlock 
			value: fieldName
			value: fieldValue ]
]

{ #category : #sending }
MailMessage >> sendOn: serverString [ 

	SMTPClient deliver: self usingServer: serverString.
]

{ #category : #initialization }
MailMessage >> setField: fieldName to: aFieldValue [
	"set a field.  If any field of the specified name exists, it will be overwritten"
	fields at: fieldName asLowercase put: (OrderedCollection with: aFieldValue).
	text := nil.
]

{ #category : #initialization }
MailMessage >> setField: fieldName toString: fieldValue [
	^self setField: fieldName to: (MIMEHeaderValue forField: fieldName fromString: fieldValue)
]

{ #category : #parsing }
MailMessage >> skipWeekdayName: aStream [
	"If the given stream starts with a weekday name or its abbreviation, advance the stream to the first alphaNumeric character following the weekday name."

	| position name abbrev |
	aStream skipSeparators.
	aStream peek isDigit
		ifTrue: [ ^ self ].
	aStream peek isLetter
		ifFalse: [ ^ self ].
	position := aStream position.
	name := (String new: 10) writeStream.
	[ aStream peek isLetter ] whileTrue: [ name nextPut: aStream next ].
	abbrev := name contents copyFrom: 1 to: (3 min: name position).
	abbrev := abbrev asLowercase.
	(#('sun' 'mon' 'tue' 'wed' 'thu' 'fri' 'sat') includes: abbrev asLowercase)
		ifTrue: [ 
			"found a weekday; skip to the next alphanumeric character"
			[ aStream peek isAlphaNumeric ] whileFalse: [ aStream skip: 1 ] ]
		ifFalse: [ 
			"didn't find a weekday so restore stream position"
			aStream position: position ]
]

{ #category : #accessing }
MailMessage >> subject [

		^(self fieldNamed: 'subject' ifAbsent: [ ^'' ])  mainValue
]

{ #category : #'fields-convenience' }
MailMessage >> subject: aString [
	self setField: 'subject' toString: aString
]

{ #category : #accessing }
MailMessage >> text [
	"the full, unprocessed text of the message"
	text ifNil: [ self regenerateText ].
	^text
]

{ #category : #accessing }
MailMessage >> time [
	| dateField |
	dateField := (self fieldNamed: 'date' ifAbsent: [ ^0 ]) mainValue.
	^ [self timeFrom: dateField] ifError: [Date today asSeconds].

]

{ #category : #parsing }
MailMessage >> timeFrom: aString [ 
	"Parse the date and time (rfc822) and answer the result as the number of seconds 
	since the start of 1980."
	| s t rawDelta delta plusOrMinus |
	s := aString readStream.

	"date part"
	t := ((self readDateFrom: s) ifNil: [ Date today ]) asSeconds.
	[ s atEnd or: [ s peek isAlphaNumeric ] ] whileFalse: [ s next ].

	"time part"
	s atEnd ifFalse: 
		[ "read time part (interpreted as local, regardless of sender's timezone)"
		s peek isDigit ifTrue: [ t := t + (Time readFrom: s) asSeconds ] ].
	s skipSeparators.

	"Check for a numeric time zone offset"
	('+-' includes: s peek) ifTrue: 
		[ plusOrMinus := s next.
		rawDelta := s peek isDigit 
			ifTrue: [ Integer readFrom: s ]
			ifFalse: [ 0 ].
		delta := (rawDelta // 100 * 60 + (rawDelta \\ 100)) * 60.
		t := plusOrMinus = $+ 
			ifTrue: [ t - delta ]
			ifFalse: [ t + delta ] ].

	"We ignore text time zone offsets like EST, GMT, etc..."
	^ t - (Date year: 1980 day: 1) asSeconds

	"MailMessage new timeFrom: 'Thu, 22 Jun 2000 14:17:47 -500'"
	"MailMessage new timeFrom: 'Thu, 22 Jun 2000 14:17:47 --500'"
	"MailMessage new timeFrom: 'on, 04 apr 2001 14:57:32'"
]

{ #category : #accessing }
MailMessage >> to [
	^self fieldsNamed: 'to' separatedBy: ', '
]

{ #category : #'fields-convenience' }
MailMessage >> to: aStringOrCollection [
	self setField: 'to' toString: aStringOrCollection asEmailHeaderString
]

{ #category : #'printing/formatting' }
MailMessage >> viewBody [
	"open a viewer on the body of this message"
	self containsViewableImage
		ifTrue: [^ self viewImageInBody].
	UIManager default  
		longMessage: self bodyTextFormatted
		title: (self name ifNil: ['(a message part)'])

]

{ #category : #'printing/formatting' }
MailMessage >> viewImageInBody [
	| stream image |
	stream := self body contentStream.
	image := Form fromBinaryStream: stream.
	(self currentWorld drawingClass withForm: image) openInWorld
]
