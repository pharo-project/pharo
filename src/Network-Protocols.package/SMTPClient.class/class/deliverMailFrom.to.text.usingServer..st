sending mail
deliverMailFrom: fromAddress to: recipientList text: messageText usingServer: aString
	"Deliver a single email to a list of users and then close the connection.  For delivering multiple messages, it is best to create a single connection and send all mail over it
	recipientList - a collection of simple internet style addresses -- no '<>' or '()' stuff
	aString - can be
		- server name and port number e.g. mail.mydomain.com:26
		- server name only e.g. mail.mydomain.com, default port is used"

	| smtpClient |
	smtpClient := self openOnHostNamed: aString.
	[smtpClient initiateSession.
	smtpClient mailFrom: fromAddress to: recipientList text: messageText.
	smtpClient quit]
		ensure: [smtpClient close]
