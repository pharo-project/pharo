tests
testRecipientList

	| message |
	message := MailMessage fromRfc822: 'To: pharo-project@lists.gforge.inria.fr, pharo-users@lists.gforge.inria.fr'.
	
	self assert: (message recipientList size = 2).
	self assert: (message recipientList first = 'pharo-project@lists.gforge.inria.fr').	
	self assert: (message recipientList second = 'pharo-users@lists.gforge.inria.fr').