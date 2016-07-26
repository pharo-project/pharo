tests - shell
testShellCommandWithStreamRedirects
	| stdOutContents | 

	self newCommand
		shellCommand: 'ps -fea | grep Pharo > /tmp/testShellCommandWithStreamRedirects.deleteme';
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: outString isEmpty. 
			stdOutContents := '/tmp/testShellCommandWithStreamRedirects.deleteme' asFileReference readStreamDo: [ :str | str contents ] .
			self assert: (stdOutContents includesSubstring: 'Pharo').						
		].
	'/tmp/testShellCommandWithStreamRedirects.deleteme' asFileReference ensureDelete. 
		 
	
	
	