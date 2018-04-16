"
a message being composed.  When finished, it will be submitted via a Celeste.
"
Class {
	#name : #MailComposition,
	#superclass : #Model,
	#instVars : [
		'messageText',
		'morphicWindow',
		'textModel'
	],
	#classVars : [
		'SmtpServer'
	],
	#category : #'Network-Mail'
}

{ #category : #'smtp server' }
MailComposition class >> isSmtpServerSet [
	^ SmtpServer notNil and: [SmtpServer notEmpty]

]

{ #category : #'instance creation' }
MailComposition class >> sendMailMessage: aMailMessage [
	| newComposition |
	newComposition := self new.
	newComposition messageText: aMailMessage text; open
]

{ #category : #'smtp server' }
MailComposition class >> setSmtpServer [
	"Set the SMTP server used to send outgoing messages via"
	SmtpServer ifNil: [SmtpServer := ''].
	SmtpServer := UIManager default
		request: 'What is your mail server for outgoing mail?'
		initialAnswer: SmtpServer.

]

{ #category : #'smtp server' }
MailComposition class >> setSmtpServer: aString [
	SmtpServer := aString
]

{ #category : #'smtp server' }
MailComposition class >> smtpServer [
	"Answer the server for sending email"

	self isSmtpServerSet
		ifFalse: [self setSmtpServer].
	SmtpServer isEmpty ifTrue: [
		self error: 'no SMTP server specified' ].

	^SmtpServer
]

{ #category : #interface }
MailComposition >> addAttachment [
	| fileSelected |
	self textModel hasUnacceptedEdits
		ifTrue: [ self textModel acceptEditsInView ].
	fileSelected := UIManager default chooseFullFileNameMatching: nil.
	fileSelected ifNil: [ ^ self ].
	fileSelected
		writeStreamDo: [ :stream | 
			stream binary.
			self
				messageText:
					((MailMessage from: self messageText asString)
						addAttachmentFrom: stream withName: fileSelected basename;
						text;
						yourself) ]
]

{ #category : #private }
MailComposition >> breakLines: aString  atWidth: width [
	"break lines in the given string into shorter lines"
	| result atAttachment |

	result := (String new: (aString size * 50 // 49)) writeStream.

	atAttachment := false.
	aString asString linesDo: [ :line | | start end | 
		(line beginsWith: '====') ifTrue: [ atAttachment := true ].
		atAttachment ifTrue: [
			"at or after an attachment line; no more wrapping for the rest of the message"
			result nextPutAll: line.  result newLine ]
		ifFalse: [
			(line beginsWith: '>') ifTrue: [
				"it's quoted text; don't wrap it"
				result nextPutAll: line. result newLine. ]
			ifFalse: [
				"regular old line.  Wrap it to multiple lines"
				start := 1.
					"output one shorter line each time through this loop"
				[ start + width <= line size ] whileTrue: [
	
					"find the end of the line"
					end := start + width - 1.
					[end >= start and: [ (line at: (end+1)) isSeparator not ]] whileTrue: [
						end := end - 1 ].
					end < start ifTrue: [
						"a word spans the entire width!"
						end := start + width - 1 ].

					"copy the line to the output"
					result nextPutAll: (line copyFrom: start to: end).
					result newLine.

					"get ready for next iteration"
					start := end+1.
					(line at: start) isSeparator ifTrue: [ start := start + 1 ].
				].

				"write out the final part of the line"
				result nextPutAll: (line copyFrom: start to: line size).
				result newLine.
			].
		].
	].

	^result contents
]

{ #category : #private }
MailComposition >> breakLinesInMessage: message [
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
]

{ #category : #interface }
MailComposition >> menuGet: aMenu shifted: shifted [
	
	aMenu addList: {
		{'Find...(f)' .	#find}.
		{'Find again (g)' . #findAgain}.
		{'Set search string (h)' . #setSearchString}.
		#-.
		{'Accept (s)' . #accept}.
		{'Send message' .  #submit}}.
	^aMenu.
]

{ #category : #access }
MailComposition >> messageText [
	"return the current text"
	^messageText.

]

{ #category : #access }
MailComposition >> messageText: aText [
	"change the current text"
	messageText := aText.
	self changed: #messageText.
	^true
]

{ #category : #interface }
MailComposition >> open [
	"open an interface"
	self openInMorphic 
]

{ #category : #interface }
MailComposition >> openInMorphic [
	"open an interface for sending a mail message with the given initial 
	text"

	| textMorph buttonsList sendButton attachmentButton |
	morphicWindow := SystemWindow labelled: 'Mister Postman'.
	morphicWindow model: self.
	textMorph := self textModel newScrolledText
		beForPlainText;
		beWrapped;
		yourself.
	textMorph menuProvider: self selector:  #menuGet:shifted:.
	morphicWindow addMorph: textMorph frame: (0 @ 0.1 corner: 1 @ 1).
	buttonsList := AlignmentMorph newRow.
	sendButton := PluggableButtonMorph on: self getState: nil action: #submit.
	sendButton
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: 'send message';
		setBalloonText: 'Accept any unaccepted edits and add this to the queue of messages to be sent';
		onColor: Color white offColor: Color white.
	buttonsList addMorphBack: sendButton.
	attachmentButton := PluggableButtonMorph on: self getState: nil action: #addAttachment.
	attachmentButton
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: 'add attachment';
		setBalloonText: 'Send a file with the message';
		onColor: Color white offColor: Color white.
	buttonsList addMorphBack: attachmentButton.
	morphicWindow addMorph: buttonsList frame: (0 @ 0 extent: 1 @ 0.1).
	morphicWindow openInWorld
]

{ #category : #interface }
MailComposition >> sendMailMessage: aMailMessage [
	self messageText: aMailMessage text
]

{ #category : #access }
MailComposition >> smtpServer [
	^self class smtpServer
]

{ #category : #access }
MailComposition >> submit [
	| message |
	"submit the message"
	self textModel hasUnacceptedEdits
		ifTrue: [ self textModel acceptEditsInView ].
	message := MailMessage from: messageText asString.
	self breakLinesInMessage: message.
	SMTPClient
		deliverMailFrom: message from
		to: (Array with: message to)
		text: message text
		usingServer: self smtpServer.
	morphicWindow ifNotNil: [ morphicWindow delete ]
]

{ #category : #access }
MailComposition >> textModel [
	^ textModel ifNil: [ textModel := RubScrolledTextModel new interactionModel: self ]
]
