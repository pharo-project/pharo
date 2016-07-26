activation
handleExit: exit for: aCommandLinehandler

	Smalltalk isInteractive 
		ifFalse: [ ^ exit pass ].
		
	exit isSuccess 
		ifFalse: [ ^ Error signal: exit messageText ].
	
	self inform: aCommandLinehandler name, ' successfully finished'.
		
	"for failing subcommands return self which is used to check if the subcommand failed"
	exit isSuccess 
		ifTrue: [ ^ aCommandLinehandler ].