tests - env
testAddAllEnvVariablesFromParent
	| command | 
	command := self newCommand
					command: '/bin/ls';
					environmentAt: 'WHATEVER' put: 'vi';
					addAllEnvVariablesFromParentWithoutOverride. 

	self assert: command envVariables size equals: (OSSUnixSubprocess new defaultEnvVariablesDictionary keys size) + 1.
	
	
