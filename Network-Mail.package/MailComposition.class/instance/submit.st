access
submit
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