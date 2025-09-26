Class {
	#name : 'PerformMessageCommandLineHandlerTest',
	#superclass : 'TestCase',
	#category : 'System-BasicCommandLineHandler-Tests-Base',
	#package : 'System-BasicCommandLineHandler-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> performCommandWithArguments: aCollection [

	^ PerformMessageCommandLineHandler new 
		commandLine: (CommandLineArguments withArguments: aCollection);
		yourself
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testCanGetTargetAndSelector [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo').
	
	command parseArguments.
	
	self assert: command targetClass equals: Object.
	self assert: command message equals: #foo.
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testCanGetTargetAndSelectorWithArguments [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo' '1' 'bar').
	
	command parseArguments.
	
	self assert: command targetClass equals: Object.
	self assert: command message equals: #foo.
	self assert: command messageArguments equals: #('1' 'bar').
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldActivateHelpWhenHelpFlagPresent [

	| command |
	command := self performCommandWithArguments: #('--help').
	
	self assert: command shouldActivateHelp
	
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldNotActivateHelpWhenHelpFlagAbsent [

	| command |
	command := self performCommandWithArguments: #('fFoo' 'bar' '--save').
	
	self deny: command shouldActivateHelp
	
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldNotSaveWhenSaveFlagAbsent [

	| command |
	command := self performCommandWithArguments: #('fFoo' 'bar').
	
	self deny: command shouldSave
	
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldSaveWhenSaveFlagPresent [

	| command |
	command := self performCommandWithArguments: #('--save').
	
	self assert: command shouldSave
	
]
