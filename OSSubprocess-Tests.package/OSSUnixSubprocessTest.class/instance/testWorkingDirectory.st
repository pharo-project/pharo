tests - pwd
testWorkingDirectory
	| oldWorkingDirectory | 

	oldWorkingDirectory := self systemAccessor getcwd.
	(FileSystem workingDirectory / 'testing') ensureCreateDirectory.
	
	self newCommand
		shellCommand: 'pwd';
		workingDirectory: (FileSystem workingDirectory / 'testing') fullName;
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: outString trimmed equals: (FileSystem workingDirectory / 'testing') fullName.	
		].
 
	self assert: self systemAccessor getcwd trimmed equals: oldWorkingDirectory trimmed.
		 
	(FileSystem workingDirectory / 'testing' ) ensureDelete.
	
