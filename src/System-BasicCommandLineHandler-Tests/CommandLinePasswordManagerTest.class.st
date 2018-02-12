"
A CommandLinePasswordManagerTest is a test class for testing the behavior of CommandLinePasswordManager
"
Class {
	#name : #CommandLinePasswordManagerTest,
	#superclass : #TestCase,
	#instVars : [
		'passwordManager'
	],
	#category : #'System-BasicCommandLineHandler-Tests'
}

{ #category : #running }
CommandLinePasswordManagerTest >> setUp [
	super setUp.
	passwordManager := CommandLinePasswordManager new
]

{ #category : #running }
CommandLinePasswordManagerTest >> tearDown [
	passwordManager := nil.
	super tearDown
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testPasswordDoesNotStorePasswordInClear [
	passwordManager password: 'test'.
	self deny: passwordManager passwordHash equals: 'test'
]
