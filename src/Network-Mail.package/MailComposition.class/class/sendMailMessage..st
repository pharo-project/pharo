instance creation
sendMailMessage: aMailMessage
	| newComposition |
	newComposition := self new.
	newComposition messageText: aMailMessage text; open