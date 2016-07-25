smtp server
setSmtpServer
	"Set the SMTP server used to send outgoing messages via"
	SmtpServer ifNil: [SmtpServer := ''].
	SmtpServer := UIManager default
		request: 'What is your mail server for outgoing mail?'
		initialAnswer: SmtpServer.
