"
A GlobalIdentifierPersistenceTest is a test class for testing the behavior of GlobalIdentifierPersistence
"
Class {
	#name : #GlobalIdentifierPersistenceTest,
	#superclass : #TestCase,
	#instVars : [
		'preferences',
		'persistence',
		'checker'
	],
	#category : #'System-Identification-Tests'
}

{ #category : #testing }
GlobalIdentifierPersistenceTest class >> isAbstract [
	^ self name = GlobalIdentifierPersistenceTest name
]

{ #category : #accessing }
GlobalIdentifierPersistenceTest >> folderName [
	^ 'pharoPreferencesTestFolder'
]

{ #category : #'instance creation' }
GlobalIdentifierPersistenceTest >> newFalseChecker [
	^ GlobalIdentifierPersistenceMockChecker new
]

{ #category : #accessing }
GlobalIdentifierPersistenceTest >> persistenceClass [
	^ self subclassResponsibility
]

{ #category : #running }
GlobalIdentifierPersistenceTest >> setUp [
	super setUp.
	self setUpPreferencesAndChecker.
	persistence := self persistenceClass new preferences: preferences; yourself.
	persistence checker: checker
]

{ #category : #running }
GlobalIdentifierPersistenceTest >> setUpPreferencesAndChecker [
	self subclassResponsibility
]

{ #category : #running }
GlobalIdentifierPersistenceTest >> tearDown [
	preferences ensureDelete.
	super tearDown
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testDefault [
	self
		deny: preferences exists
		description: 'The preference file should not exist by default.'.
	self
		deny: preferences parent exists
		description: 'The preference directory should not exist by default.'
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testDelete [
	preferences parent ensureCreateDirectory.
	preferences ensureCreateFile.
	self assert: preferences exists.
	persistence delete.
	self deny: preferences exists
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testEnsureDirectory [
	persistence ensureDirectory.
	self assert: preferences parent exists.
	self deny: preferences exists
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testIsEnabled [
	self assert: persistence isEnabled
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testIsEnabled2 [
	persistence checker: self newFalseChecker.
	self deny: persistence isEnabled
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testLoad [
	"Load when nothing is stored."
	| values identifier |

	identifier := GlobalIdentifier basicNew initialize.
	values := identifier persistedInformation.
	values at: #a put: 42.

	persistence load: identifier.

	"It has the saved value, the computerUUID and the secretUUID"
	self assert: values size equals: 3.
	self assert: (values at: #a) equals: 42.
	self assert: preferences exists
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testLoad2 [
	"Load stored values."
	| identifier otherIdentifier |
	identifier := GlobalIdentifier basicNew initialize.
	otherIdentifier := GlobalIdentifier basicNew initialize.

	identifier persistedInformation at: #a put: 42.
	persistence save: identifier persistedInformation.

	persistence load: otherIdentifier.
	self assert: (otherIdentifier persistedInformation at: #a) equals: 42.
	self assert: (otherIdentifier persistedInformation includesKey: #computerUUID).
	self assert: (otherIdentifier persistedInformation includesKey: #secretUUID)
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testLoad3 [
	"Load stored values."

	| values loaded |
	persistence checker: self newFalseChecker.
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	loaded := Dictionary new.
	persistence load: loaded.
	self assertEmpty: loaded
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testSave [
	| values |
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	self assert: preferences exists
]

{ #category : #tests }
GlobalIdentifierPersistenceTest >> testSave2 [
	| values |
	persistence checker: self newFalseChecker.
	values := Dictionary new.
	values at: #a put: 42.
	persistence save: values.
	self deny: preferences exists
]
