"
A GlobalIdentifierFuelPersistenceTest is a test class for testing the behavior of GlobalIdentifierFuelPersistence
"
Class {
	#name : #GlobalIdentifierFuelPersistenceTest,
	#superclass : #GlobalIdentifierPersistenceTest,
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierFuelPersistenceTest >> persistenceClass [
	^ GlobalIdentifierFuelPersistence
]

{ #category : #running }
GlobalIdentifierFuelPersistenceTest >> setUpPreferencesAndChecker [
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / 'preferences.fuel'.
	preferences ensureDelete.
	checker := GlobalIdentifierPersistenceMockChecker new answer: [ FileLocator workingDirectory exists ].

]

{ #category : #running }
GlobalIdentifierFuelPersistenceTest >> tearDown [
	(FileLocator workingDirectory / self folderName) 
		ensureDeleteAllChildren; 
		ensureDelete.
	super tearDown 
]
