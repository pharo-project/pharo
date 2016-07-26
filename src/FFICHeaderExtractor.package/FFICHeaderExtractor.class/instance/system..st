primitives
system: aCommandString
	"Executes aCommandString in the OS via the system() call."
	| returnValue |
	returnValue := self primitiveSystem: aCommandString.
	(returnValue = -1) ifTrue: [ 
		self perror: 'system()'	
	].
	^ returnValue 