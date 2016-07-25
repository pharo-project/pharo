printing/formatting
bodyTextFormatted
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