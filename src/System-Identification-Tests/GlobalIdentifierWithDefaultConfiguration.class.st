"
SUnit tests for GlobalIdentifier with a default configuration
"
Class {
	#name : #GlobalIdentifierWithDefaultConfiguration,
	#superclass : #GlobalIdentifierTest,
	#instVars : [
		'oldPreferences'
	],
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierWithDefaultConfiguration >> oldPreferencesFileName [
	^ identifier persistenceStrategy previous fileName
]

{ #category : #running }
GlobalIdentifierWithDefaultConfiguration >> setUpPersistenceStrategyAndChecker [
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self actualPreferencesFileName.
	preferences ensureDelete.
	oldPreferences := FileLocator workingDirectory / self folderName / 'pharoX' / self oldPreferencesFileName.
	oldPreferences ensureDelete.
	identifier persistenceStrategy preferences: preferences.
	identifier persistenceStrategy previous preferences: oldPreferences.
	identifier persistenceStrategy checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).
	identifier persistenceStrategy previous checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).
	
]

{ #category : #running }
GlobalIdentifierWithDefaultConfiguration >> tearDown [
	oldPreferences ensureDelete.
	super tearDown
]

{ #category : #tests }
GlobalIdentifierWithDefaultConfiguration >> testEnsure [
	| stored |
	super testEnsure.
	self assert: oldPreferences exists description: 'The old preferences should be stored now.'.
	stored := identifier persistenceStrategy previous load.
	self assert: stored equals: identifier persistedInformation.
]
