tests
testSelectSourceFiles
	| codeLoader commandLine |

	commandLine := CommandLineArguments withArguments: {'does-not-exist.st'. '--host=localhost'}.	
	codeLoader := STCommandLineHandler new
		commandLine: commandLine;
		yourself.

	"Verify that 'does-not-exist.st' is loaded but not --host=localhost"
	codeLoader selectSourceFiles.

	"Now verify that it doesn't try to load localhost"
	self assert: (codeLoader instVarNamed: 'sourceFiles') notNil.
	self assert: (codeLoader instVarNamed: 'sourceFiles') size equals: 1.
	self assert: (codeLoader instVarNamed: 'sourceFiles') first name equals: 'does-not-exist.st'.