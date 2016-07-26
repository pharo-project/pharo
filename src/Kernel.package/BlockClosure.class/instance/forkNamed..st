scheduling
forkNamed: aString

	"Create and schedule a Process running the code in the receiver and

	having the given name."



	^ self newProcess name: aString; resume