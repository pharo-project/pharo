Class {
	#name : 'SaveCliCommandTest',
	#superclass : 'TestCase',
	#instVars : [
		'context'
	],
	#category : 'Clap-Tests-Commands',
	#package : 'Clap-Tests',
	#tag : 'Commands'
}

{ #category : 'tests' }
SaveCliCommandTest >> testIsValidImageNameIsFalseWhenImageNameEmpty [
	| command |
	context := SaveCliCommand commandSpecification activationWith: #('save' '').
	command := context command.
	
	self deny: command isValidImageName 
]

{ #category : 'tests' }
SaveCliCommandTest >> testIsValidImageNameIsFalseWhenNoImageNameProvided [
	| command |
	context := SaveCliCommand commandSpecification activationWith: #('save').
	command := context command.
	
	self deny: command isValidImageName 
]

{ #category : 'tests' }
SaveCliCommandTest >> testShoudlDeleteOldImageWhenDeleteOldFlagPresent [
	| command |
	context := SaveCliCommand commandSpecification activationWith: #('save' '--delete-old' 'new').
	command := context command.
	
	self assert: command shouldDeleteOldImage
]

{ #category : 'tests' }
SaveCliCommandTest >> testShoudlNotDeleteOldImageWhenDeleteOldFlagAbsent [
	| command |
	context := SaveCliCommand commandSpecification activationWith: #('save' 'new').
	command := context command.
	
	self deny: command shouldDeleteOldImage
]
