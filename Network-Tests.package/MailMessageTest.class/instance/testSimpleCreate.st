tests
testSimpleCreate

	| message |
	message := MailMessage
		from: 'community@world.st'
		to: { 'pharo-project@lists.gforge.inria.fr'. 'pharo-users@lists.gforge.inria.fr' }
		about: 'Great Progress'
		asFollows: 'Pharo is getting so cool!'.
	
	self assert: (message text matchesRegex: 'Date\: [[:alpha:]]+, \d+ [[:alpha:]]+ \d+ \d\d\:\d\d\:\d\d [-+]\d\d\d\d
From\: community@world.st
To\: pharo-project@lists.gforge.inria.fr, pharo-users@lists.gforge.inria.fr
Subject\: Great Progress

Pharo is getting so cool!
').