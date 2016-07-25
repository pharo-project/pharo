I am ZdcSecureSMTPClient.

I open a ZdcSecureSocketStream to the SMTP server and connect it at the SSL level.

| mailMessage |
mailMessage := MailMessage empty.
mailMessage setField: 'subject' toString: 'ZdcSecureSMTPClient Test'.
mailMessage body: (MIMEDocument contentType: 'text/plain' content: 'This is test from Pharo Smalltalk').
ZdcSecureSMTPClient
	sendUsingGMailAccount: '<your-name>@gmail.com' 
	password: '<your-password>'
	to: '<email-address>' 
	message: mailMessage