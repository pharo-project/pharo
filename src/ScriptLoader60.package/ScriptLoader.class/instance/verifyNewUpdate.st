menu integration actions
verifyNewUpdate
	self repository: self class defaultMCWaitingFolder.
	self class loadLatestPackage: 'ScriptLoader' fromRepository: self repository.
	self perform: ('update' , self getLatestUpdateNumber asString) asSymbol.
	(self confirm: 'Completed loading the new update. Run all test now?')
		ifTrue: [ 
			Author fullName: 'tester'.
			Smalltalk globals at: #TestRunner ifPresent: [ :class | class open model runAll ] ]