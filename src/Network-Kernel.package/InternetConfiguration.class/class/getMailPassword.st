lookups
getMailPassword 
	"Return the mail account Password "
	"InternetConfiguration getMailPassword "

	^self primitiveGetStringKeyedBy: 'MailPassword'
