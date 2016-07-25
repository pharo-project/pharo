lookups
getSMTPHost
	"Return the SMTP server"
	"InternetConfiguration getSMTPHost"

	^self primitiveGetStringKeyedBy: 'SMTPHost'
