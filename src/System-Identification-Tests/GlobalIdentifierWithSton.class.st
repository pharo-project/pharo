"
SUnit tests for GlobalIdentifier with STON
"
Class {
	#name : #GlobalIdentifierWithSton,
	#superclass : #GlobalIdentifierTest,
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierWithSton >> actualPreferencesFileName [
	^ 'preferences.ston'
]

{ #category : #running }
GlobalIdentifierWithSton >> setUpPersistenceStrategyAndChecker [
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self actualPreferencesFileName.
	preferences ensureDelete.
	identifier persistenceStrategy: GlobalIdentifierPersistence ston.
	identifier persistenceStrategy preferences: preferences.
	identifier persistenceStrategy checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).

]
