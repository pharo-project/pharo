private
breakLinesInMessage: message
	"reformat long lines in the specified message into shorter ones"

	"multipart message; process the top-level parts.  HACK: the parts are modified in place"

	message body mainType = 'text'
		ifTrue: [ 
			| newBodyText "it's a single-part text message.  reformat the text" |
			newBodyText := self breakLines: message bodyText atWidth: 72.
			message body: (MIMEDocument contentType: message body contentType content: newBodyText).
			^ self ].
	message body isMultipart
		ifFalse: [ ^ self ].
	message parts
		do: [ :part | 
			part body mainType = 'text'
				ifTrue: [ 
					| newBodyText |
					newBodyText := self breakLines: part bodyText atWidth: 72.
					part body: (MIMEDocument contentType: part body contentType content: newBodyText) ] ].
	message regenerateBodyFromParts