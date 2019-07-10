"
A CommandLinePasswordManagerTest is a test class for testing the behavior of CommandLinePasswordManager
"
Class {
	#name : #CommandLinePasswordManagerTest,
	#superclass : #TestCase,
	#instVars : [
		'passwordManager'
	],
	#category : #'System-BasicCommandLineHandler-Tests-Management'
}

{ #category : #running }
CommandLinePasswordManagerTest >> setUp [
	super setUp.
	passwordManager := CommandLinePasswordManager new
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testCannotChangeNumberOfIterationsWhenPasswordIsSet [
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: nil numberOfHashIterations: 1.
	self should: [ passwordManager numberOfHashIterations: 2 ] raise: Error
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testCannotChangePepperWhenPasswordIsSet [
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: 'test' numberOfHashIterations: nil.
	self should: [ passwordManager hashingPepper: 'test2' ] raise: Error
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testHasPasswordSet [
	self deny: passwordManager hasPasswordSet.
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: nil numberOfHashIterations: nil.
	self assert: passwordManager hasPasswordSet
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testHashingPepper [
	| hashWithPreviousPepper |
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: 'pepper' numberOfHashIterations: nil.
	hashWithPreviousPepper := passwordManager passwordHash.
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: 'notSalt' numberOfHashIterations: nil.
	self deny: hashWithPreviousPepper equals: passwordManager passwordHash
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testIsMatchingPassword [
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: nil numberOfHashIterations: nil.
	self assert: (passwordManager isMatchingPassword: 'test')
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testNumberOfHashIterations [
	| hashWithOneIteration |
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: nil numberOfHashIterations: 1.
	hashWithOneIteration := passwordManager passwordHash.
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: nil numberOfHashIterations: 2.
	self deny: hashWithOneIteration equals: passwordManager passwordHash
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testPasswordDoesNotStorePasswordInClear [
	passwordManager password: 'test'.
	self deny: passwordManager passwordHash equals: 'test'
]

{ #category : #tests }
CommandLinePasswordManagerTest >> testRemovePasswordProtection [
	passwordManager protectCommandLinesByPasswordWith: 'test' pepper: nil numberOfHashIterations: nil.
	self assert: passwordManager hasPasswordSet.
	passwordManager removePasswordProtection.
	self deny: passwordManager hasPasswordSet
]
