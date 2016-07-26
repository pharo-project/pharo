tests - env
testInheritVarFromEnv
	| command | 
	command := self newCommand
					shellCommand: 'echo ${HOME} 2>&1 > /tmp/testReadUserDefinedEnvVar ';
					runAndWait.
	
	self assert: ('/tmp/testReadUserDefinedEnvVar' asFileReference readStreamDo: [ :str | str contents ]) trimmed equals: (Smalltalk os environment at: 'HOME') trimmed.
	'/tmp/testReadUserDefinedEnvVar' asFileReference ensureDelete. 