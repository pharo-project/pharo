Class {
	#name : 'ClapPharoPerformCommandTest',
	#superclass : 'ClapPharoCommandsTest',
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testCanGetArguments [
	context := ClapPharoPerformCommand commandSpecification activationWith: #('perform' 'Array' 'with:with:' 'a' 'b'). 
	
	self
		assertCollection: context command arguments
		equals: #('a' 'b')
]

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testCanGetKnownClass [
	context := ClapPharoPerformCommand commandSpecification activationWith: #('perform' 'String' 'two'). 
	
	self
		assert: context command targetClass
		equals: String
]

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testCanGetSelector [
	context := ClapPharoPerformCommand commandSpecification activationWith: #('perform' 'String' 'empty'). 
	
	self
		assert: context command selector
		equals: 'empty'
]

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testExecution [
	| result |
	context := ClapPharoPerformCommand commandSpecification activationWith: #('perform' 'Array' 'with:with:' 'a' 'b').
	
	result := context command execute.
	
	self
		assert: result
		equals: #('a' 'b')
]

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testNoArgumentExits [
	context := ClapPharoPerformCommand commandSpecification activateWith: #('perform'). 
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing className or selector'
]

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testOneArgumentExits [
	context := ClapPharoPerformCommand commandSpecification activateWith: #('perform' 'one'). 
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Missing className or selector'
]

{ #category : 'tests' }
ClapPharoPerformCommandTest >> testUnknownClassExits [
	context := ClapPharoPerformCommand commandSpecification activateWith: #('perform' 'one' 'two'). 
	
	self assertFailure.
	self
		assert: context exitException messageText
		equals: 'Unknown class'
]
