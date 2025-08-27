Class {
	#name : 'ClapPharoFixMethodsCommandTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'running' }
ClapPharoFixMethodsCommandTest >> setUp [
	super setUp.

	self class compile: 'foo'.
]

{ #category : 'running' }
ClapPharoFixMethodsCommandTest >> tearDown [ 

	(self class >> #foo) removeFromSystem.
	super tearDown

]

{ #category : 'tests' }
ClapPharoFixMethodsCommandTest >> testActivation [

	context := ClapPharoBootstrapCommand commandSpecification
		           activationWith: #( 'bootstrap' 'fixMethods' ).

	self 
		assert: context command class 
		equals: ClapPharoFixMethodsCommand
]

{ #category : 'tests' }
ClapPharoFixMethodsCommandTest >> testCanFixMethodPackage [

	| command |
	command := ClapPharoFixMethodsCommand new.
	self deny: (self class >> #foo) protocol name equals: #someProtocol.
	
	command fixMethod: {
			self class name.
			false.
			#foo.
			#someProtocol }.

	self assert: (self class >> #foo) protocol name equals: #someProtocol
]
