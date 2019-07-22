Class {
	#name : #STCommandLineHandlerTest,
	#superclass : #ClassTestCase,
	#category : #'System-BasicCommandLineHandler-Tests-Base'
}

{ #category : #coverage }
STCommandLineHandlerTest >> classToBeTested [

	^ STCommandLineHandler
]

{ #category : #tests }
STCommandLineHandlerTest >> testCommandLineHandlerCondition [
	| commandLine reference |
	
	commandLine := CommandLineArguments withArguments: {'/non/existing/file.st'}. 
	"the reponsibility is transfered tot the BasicCodeLoader as soon as there is a .st file in the arguments"
	self assert: (STCommandLineHandler isResponsibleFor: commandLine).
		
	[ reference := FileSystem disk workingDirectory / 'codeLoad1.st'.
	  reference writeStreamDo: [ :stream | stream nextPutAll: '42' ].
	  commandLine := CommandLineArguments withArguments: {reference fullName}. 
	  self assert: (STCommandLineHandler isResponsibleFor: commandLine) ]
	ensure: [ reference delete ]

]

{ #category : #tests }
STCommandLineHandlerTest >> testInstallSourceFilesFail [
	| codeLoader |
	
	codeLoader := STCommandLineHandler new.
	codeLoader loadSourceFiles: {'codeLoad1.st'}.
	"Ensure file is not there"
	(FileSystem disk workingDirectory / 'codeLoad1.st') asFileReference ensureDelete. 
	"Trying non existing file"
	self should: [ codeLoader installSourceFiles ] raise: Error
]

{ #category : #tests }
STCommandLineHandlerTest >> testLoadSourceFiles [
	| codeLoader |
	
	codeLoader := STCommandLineHandler new.
	
	codeLoader loadSourceFiles: {
		'/not/existing/codeLoad1.st'. 
		'/not/existing/codeLoad2.st'}.
	
	self assert: (codeLoader instVarNamed: 'sourceFiles') notNil.
	self assert: (codeLoader instVarNamed: 'sourceFiles') size equals: 2.
	self assert: ((codeLoader instVarNamed: 'sourceFiles') allSatisfy: [ :each | each isKindOf: File ])
]

{ #category : #tests }
STCommandLineHandlerTest >> testSelectSourceFiles [
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
	self assert: (codeLoader instVarNamed: 'sourceFiles') first name equals: 'does-not-exist.st'
]
