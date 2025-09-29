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

{ #category : 'message performing' }
PerformMessageCommandLineHandlerTest >> performTest [

	[ super performTest ]
	on: Exit do: [ :exit | self fail: 'Exit with message: ', exit messageText ]
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testCanGetEmptyCommandOptionsWhenNoCommandOptionSpecified [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo' '1' 'bar').
	
	command parseArguments.
	
	self assert: command commandOptions isEmpty.
	self assert: command messageArguments equals: #('1' 'bar').
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
PerformMessageCommandLineHandlerTest >> testCanGetTargetAndSelectorWithArrayOfArguments [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo' '--' '1' 'bar').
	
	command parseArguments.
	
	self assert: command targetClass equals: Object.
	self assert: command message equals: #foo.
	self assert: command messageArguments equals: #( #('1' 'bar') ).
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testMessageArgumentsFollowingArrayWrappingFlagAreWrappedInAnArray [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo' '1' '--' '2' 'bar').
	
	command parseArguments.
	
	self assert: command messageArguments equals: #( '1' #('2' 'bar') ).
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testMessageArgumentsIsAnArrayOfProvidedArgumentsWhenNoArrayWrappingFlag [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo' '1' 'bar').
	
	command parseArguments.
	
	self assert: command messageArguments equals: #('1' 'bar').
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testMessageArgumentsIsAnEmptyArrayWhenNoArgumentProvided [

	| command |
	command := self performCommandWithArguments: #('Object' 'foo').
	
	command parseArguments.
	
	self assert: command messageArguments equals: #()
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testMessageArgumentsIsAnEmptyArrayWhenNoArgumentProvidedAndArrayWrappingFlagPresent [

	| command |
	command := self performCommandWithArguments: #('--bar' 'Object' 'foo' '--').
	
	command parseArguments.
	
	self assert: command messageArguments equals: #( #() ).
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldActivateHelpWhenHelpFlagPresent [

	| command |
	command := self performCommandWithArguments: #('--help' '--').
	
	self assert: command shouldActivateHelp
	
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldNotActivateHelpWhenHelpFlagAbsent [

	| command |
	command := self performCommandWithArguments: #( '--save' 'Object' 'bar').
	
	command parseArguments.
	
	self deny: command shouldActivateHelp
	
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldNotSaveWhenSaveFlagAbsent [

	| command |
	command := self performCommandWithArguments: #('Object' 'bar').
	
	command parseArguments.
	
	self deny: command shouldSave
	
]

{ #category : 'tests' }
PerformMessageCommandLineHandlerTest >> testShouldSaveWhenSaveFlagPresent [

	| command |
	command := self performCommandWithArguments: #('--save' 'Object' 'foo').
	
	command parseArguments.
	
	self assert: command shouldSave
	
]
