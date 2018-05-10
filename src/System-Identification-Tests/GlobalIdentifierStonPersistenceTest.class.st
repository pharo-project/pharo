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
	memory := FileSystem memory.
	preferences := memory workingDirectory / self folderName / 'pharoX' / 'preferences.ston'.
	preferences ensureDelete.
	checker := GlobalIdentifierPersistenceMockChecker new answer: [ memory workingDirectory exists ].

]
