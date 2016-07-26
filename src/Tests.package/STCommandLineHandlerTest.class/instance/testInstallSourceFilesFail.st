tests
testInstallSourceFilesFail
	| codeLoader |
	
	codeLoader := STCommandLineHandler new.
	codeLoader loadSourceFiles: {'codeLoad1.st'}.
	"Ensure file is not there"
	(FileSystem disk workingDirectory / 'codeLoad1.st') asFileReference ensureDelete. 
	"Trying non existing file"
	self should: [ codeLoader installSourceFiles ] raise: Error.