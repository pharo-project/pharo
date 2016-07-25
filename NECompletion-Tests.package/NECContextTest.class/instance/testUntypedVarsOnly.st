tests
testUntypedVarsOnly
	self checkUntypedVariablesOnly: 'testIt
	'.
	self checkUntypedVariablesOnly: '+ aTest
	'.
	self checkUntypedVariablesOnly: 'gaga: aTest
	'.
	self checkUntypedVariablesOnly: 'gaga
	^ '.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ '.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ :each'.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ :each |'.
	self checkUntypedVariablesOnly: 'testIt 
	a ifTrue:[ :each | '.
	self checkUntypedVariablesOnly: 'testIt
	<primitive: 67> 
	'.
	self checkUntypedVariablesOnly: 'testIt
	ab _'.
	self checkUntypedVariablesOnly: 'testIt
	ab _ '.
	self checkUntypedVariablesOnly: 'self compare: '.
	self checkUntypedVariablesOnly: 'self compare: x caseSensitive: '.
	self checkUntypedVariablesOnly: 'self gaga: x gugu: ('.
	self checkUntypedVariablesOnly: 'testIt
	a _ 15 + '.
	self checkUntypedVariablesOnly: 'testIt
	self hugatada: '