lookups
getMailAccount
	"Return the mail account user@host.domain"
	"InternetConfiguration getMailAccount"

	^self primitiveGetStringKeyedBy: 'MailAccount'
