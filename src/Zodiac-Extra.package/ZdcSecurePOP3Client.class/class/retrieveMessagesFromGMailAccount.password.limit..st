example
retrieveMessagesFromGMailAccount: accountAddress password: password limit: limit
	| pop3Client messages messageCount |
	(pop3Client := self openOnHostNamed: 'smtp.gmail.com' port: self defaultPortForSSL)
		loginUser: accountAddress password: password.
	messages := OrderedCollection new.
	messageCount := pop3Client messageCount.
	messageCount to: ((messageCount - limit) max: 0) + 1 by: -1 do: [ :each |
		messages add: (pop3Client retrieveMessage: each) ].
	pop3Client
		quit;
		close.
	^ messages
