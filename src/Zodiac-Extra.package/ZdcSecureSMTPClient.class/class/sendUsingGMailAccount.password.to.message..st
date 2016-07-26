example
sendUsingGMailAccount: senderAddress password: password to: receiverAddress message: mailMessage
	| smtpClient |
	(smtpClient := self new)
		user: senderAddress; 
		password: password.
	smtpClient openOnHost: (NetNameResolver addressForName: 'smtp.gmail.com') port: 465.
	mailMessage setField: 'from' toString: senderAddress.
	mailMessage setField: 'to' toString: receiverAddress.
	^ smtpClient 
		mailFrom: senderAddress to: { receiverAddress } text: mailMessage text;
		quit; 
		close;
		yourself
