smtp server
isSmtpServerSet
	^ SmtpServer notNil and: [SmtpServer notEmpty]
