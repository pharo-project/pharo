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
