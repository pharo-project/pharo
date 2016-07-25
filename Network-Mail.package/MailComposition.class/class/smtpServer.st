smtp server
smtpServer
	"Answer the server for sending email"

	self isSmtpServerSet
		ifFalse: [self setSmtpServer].
	SmtpServer isEmpty ifTrue: [
		self error: 'no SMTP server specified' ].

	^SmtpServer