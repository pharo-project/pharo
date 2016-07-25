tests
testBasicCommandlineHandler
	
	| args |
	args := self argumentsWith: #().
	"BasicCommandLineHandler gets activated by default on image startup, so there is no need to activate it in a nother case"
	self deny: (BasicCommandLineHandler isResponsibleFor: args).
	"The BasicCommandLineHandler should always delegate to the PharoCommandLineHandler if it is present"
	self 
		assert: BasicCommandLineHandler new selectedHandler
		equals: PharoCommandLineHandler