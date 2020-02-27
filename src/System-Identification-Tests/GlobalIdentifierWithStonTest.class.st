"
SUnit tests for GlobalIdentifier with STON
"
Class {
	#name : #GlobalIdentifierWithStonTest,
	#superclass : #GlobalIdentifierTest,
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierWithStonTest >> actualPreferencesFileName [
	^ 'preferences.ston'
]

{ #category : #running }
GlobalIdentifierWithStonTest >> setUpPersistenceStrategyAndChecker [
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self actualPreferencesFileName.
	preferences ensureDelete.
	identifier persistenceStrategy: GlobalIdentifierPersistence ston.
	identifier persistenceStrategy preferences: preferences.
	identifier persistenceStrategy checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).

]
