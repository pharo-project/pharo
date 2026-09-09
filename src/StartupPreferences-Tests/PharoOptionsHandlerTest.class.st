"
A PharoOptionsHandlerTest is a test class for testing the behavior of PharoOptionsHandler
"
Class {
	#name : 'PharoOptionsHandlerTest',
	#superclass : 'TestCase',
	#instVars : [
		'noDefaultPreferencesArgument',
		'preferencesFileArgument'
	],
	#category : 'StartupPreferences-Tests',
	#package : 'StartupPreferences-Tests'
}

{ #category : 'tests' }
PharoOptionsHandlerTest >> noDefaultPreferencesArgument [

	^ noDefaultPreferencesArgument ifNil: [ noDefaultPreferencesArgument := PharoOptionsHandler new noDefaultPreferencesArgument ]
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> preferencesFileArgument [

	^ preferencesFileArgument ifNil: [ preferencesFileArgument := PharoOptionsHandler new preferencesFileArgument ]
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanFilterNoDefaultPreferencesFlagWhenFlagIsFirst [

	| optionsHandler |
	optionsHandler := PharoOptionsHandler forArguments: { self noDefaultPreferencesArgument . 'foo' . 'bar' }.

	self 
		assert: optionsHandler filteredArguments 
		equals: #('foo' 'bar')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanFilterNoDefaultPreferencesFlagWhenFlagIsLast [

	| optionsHandler |
	optionsHandler := PharoOptionsHandler forArguments: { 'foo' . 'bar' . self noDefaultPreferencesArgument }.

	self 
		assert: optionsHandler filteredArguments 
		equals: #('foo' 'bar')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanFilterNoDefaultPreferencesFlagWhenFlagIsNotFirstOrLast [

	| optionsHandler |
	optionsHandler := PharoOptionsHandler forArguments: { 'foo' . self noDefaultPreferencesArgument . 'bar' }.

	self 
		assert: optionsHandler filteredArguments 
		equals: #('foo' 'bar')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanFilterPreferencesFileWhenArgumentIsFirst [

	| optionsHandler |
	optionsHandler := PharoOptionsHandler forArguments: { self preferencesFileArgument,'=myfile' . 'foo' . 'bar' }.

	self 
		assert: optionsHandler filteredArguments 
		equals: #('foo' 'bar')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanFilterPreferencesFileWhenArgumentIsLast [

	| optionsHandler |
	optionsHandler := PharoOptionsHandler forArguments: { 'foo' . 'bar' . self preferencesFileArgument,'=myfile' }.

	self 
		assert: optionsHandler filteredArguments 
		equals: #('foo' 'bar')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanFilterPreferencesFileWhenArgumentIsNotFirstOrLast [

	| optionsHandler |
	optionsHandler := PharoOptionsHandler forArguments: { 'foo' . self preferencesFileArgument,'=myfile' . 'bar' }.

	self 
		assert: optionsHandler filteredArguments 
		equals: #('foo' 'bar')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanHandleInteractiveFlag [

	| arguments handler |
	arguments := #('--interactive' 'foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	
	self assert: handler filteredArguments equals: #('foo' '--help')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanHandleNoDefaultPreferences [

	| arguments handler |
	arguments := #('--no-default-preferences' 'foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	

	self assert: handler isOmittingPreferences.
	self deny: handler isOverridingPreferences.
	self assert: handler filteredArguments equals: #('foo' '--help')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testCanHandlePreferencesFile [

	| arguments handler |
	arguments := #('--preferences-file=prefs.file' 'foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	

	self deny: handler isOmittingPreferences.
	self assert: handler isOverridingPreferences.
	self assert: handler preferencesFile basename equals: 'prefs.file'.
	self assert: handler filteredArguments equals: #('foo' '--help')
]

{ #category : 'tests' }
PharoOptionsHandlerTest >> testPharoCommandLineHandlerDoesNotRemoveArgumentsIfNoPreferencesArgument [

	| arguments handler |
	arguments := #('foo' '--help').
	handler := PharoOptionsHandler forArguments: arguments.
	

	self deny: handler isOmittingPreferences.
	self deny: handler isOverridingPreferences.
	self assert: handler filteredArguments equals: #('foo' '--help')
]
