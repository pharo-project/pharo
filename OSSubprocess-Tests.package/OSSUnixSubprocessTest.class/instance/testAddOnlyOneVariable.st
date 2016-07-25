tests - env
testAddOnlyOneVariable
	| command | 
	command := self newCommand
					command: '/bin/ls';
					environmentAt: 'EDITOR' put: 'vi'.

	self assert: command envVariables size equals: 1.
	
	
