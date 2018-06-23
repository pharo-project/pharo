"
SUnit tests for GlobalIdentifierStonPersistence
"
Class {
	#name : #GlobalIdentifierStonPersistenceTest,
	#superclass : #GlobalIdentifierPersistenceTest,
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierStonPersistenceTest >> persistenceClass [
	^ GlobalIdentifierStonPersistence
]

{ #category : #running }
GlobalIdentifierStonPersistenceTest >> setUpPreferencesAndChecker [
	| memory |
	memory := FileSystem newMemory.
	preferences := memory workingDirectory / self folderName / 'pharoX' / 'preferences.ston'.
	preferences ensureDelete.
	checker := GlobalIdentifierPersistenceMockChecker new answer: [ memory workingDirectory exists ]
]

{ #category : #running }
GlobalIdentifierStonPersistenceTest >> tearDown [
	(FileSystem memory workingDirectory / self folderName) ensureDeleteAll.
	super tearDown
]
