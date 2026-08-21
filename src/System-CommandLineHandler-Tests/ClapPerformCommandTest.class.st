Class {
	#name : 'ClapPerformCommandTest',
	#superclass : 'ClapPharoCommandsTest',
	#instVars : [
		'command'
	],
	#category : 'System-CommandLineHandler-Tests-Base',
	#package : 'System-CommandLineHandler-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
ClapPerformCommandTest >> activationWith: arguments [
	
	context := ClapPerformCommand commandSpecification activationWith: arguments.
	command := context command
]

{ #category : 'tests' }
ClapPerformCommandTest >> testAllArguments [
	context := ClapPerformCommand commandSpecification activationWith: #( 'perform' '--save' 'Foo' 'Bar' '--as-array' '1' 'bar' ).
	
	[ context match; validateAll ]
		on: Exit 
		do: [ :exit | self fail: 'Validation failed' ].
		
	self assert: context isMatch.
	self assert: context command allArguments equals: #( '--save' 'Foo' 'Bar' '--as-array' '1' 'bar' ).
]

{ #category : 'tests' }
ClapPerformCommandTest >> testCanGetTargetAndSelector [
	self activationWith: #('perform' 'Object' 'foo').
	
	self assert: command targetClass equals: 'Object'.
	self assert: command messageSelector equals: 'foo'.
]

{ #category : 'tests' }
ClapPerformCommandTest >> testCanGetTargetAndSelectorWithArguments [
	self activationWith: #('perform' 'Object' 'foo' '1' 'bar').
	
	self assert: command targetClass equals: 'Object'.
	self assert: command messageSelector equals: #foo.
	self assert: command messageArguments asArray equals: #('1' 'bar')
]

{ #category : 'tests' }
ClapPerformCommandTest >> testCanGetTargetAndSelectorWithArrayOfArguments [
	self activationWith: #('perform' 'Object' 'foo' '--as-array' '1' 'bar').
	
	self assert: command targetClass equals: 'Object'.
	self assert: command messageSelector equals: #foo.
	self assert: command messageArgumentsAsArray asArray equals: #('1' 'bar')
]

{ #category : 'tests' }
ClapPerformCommandTest >> testHelp [
	context := ClapPerformCommand commandSpecification activateWith: #('perform' '--help').
	
	self assertSuccess.
	self
		assert: (self outputString beginsWith: 'Performs a message on a given class or global object.')
]

{ #category : 'tests' }
ClapPerformCommandTest >> testSaveFlag [

	context := ClapPerformCommand commandSpecification activationWith: #( 'perform' '--save' 'Foo' 'Bar' ).

	self assert: context command shouldSave
]
