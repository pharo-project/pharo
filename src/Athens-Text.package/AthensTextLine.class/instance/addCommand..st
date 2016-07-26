as yet unclassified
addCommand: aCommand

	commands 
		ifNil: [ tail := commands := aCommand. ]
		ifNotNil: [ tail next: aCommand ].
	tail := tail tail.
