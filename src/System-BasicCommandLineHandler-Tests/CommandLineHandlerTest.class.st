Class {
	#name : 'CommandLineHandlerTest',
	#superclass : 'TestCase',
	#category : 'System-BasicCommandLineHandler-Tests-Base',
	#package : 'System-BasicCommandLineHandler-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
CommandLineHandlerTest >> testCanHandleInteractiveFlag [

	| arguments handler |
	arguments := #('--interactive' 'foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	
	self assert: handler filteredArguments equals: #('foo' '--help')
]

{ #category : 'tests' }
CommandLineHandlerTest >> testCanHandleNoDefaultPreferences [

	| arguments handler |
	arguments := #('--no-default-preferences' 'foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	

	self assert: handler isOmittingPreferences.
	self deny: handler isOverridingPreferences.
	self assert: handler filteredArguments equals: #('foo' '--help')
]

{ #category : 'tests' }
CommandLineHandlerTest >> testCanHandlePreferencesFile [

	| arguments handler |
	arguments := #('--preferences-file=prefs.file' 'foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	

	self deny: handler isOmittingPreferences.
	self assert: handler isOverridingPreferences.
	self assert: handler preferencesFile basename equals: 'prefs.file'.
	self assert: handler filteredArguments equals: #('foo' '--help')
]

{ #category : 'tests' }
CommandLineHandlerTest >> testPharoCommandLineHandlerDoesNotRemoveArgumentsIfNoPreferencesArgument [

	| arguments handler |
	arguments := #('foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	

	self deny: handler isOmittingPreferences.
	self deny: handler isOverridingPreferences.
	self assert: handler filteredArguments equals: #('foo' '--help')
]
