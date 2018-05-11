"
SUnit tests for GlobalIdentifier with FUEL
"
Class {
	#name : #GlobalIdentifierWithFuel,
	#superclass : #GlobalIdentifierTest,
	#category : #'System-Identification-Tests'
}

{ #category : #accessing }
GlobalIdentifierWithFuel >> actualPreferencesFileName [
	^ 'preferences.fuel'
]

{ #category : #running }
GlobalIdentifierWithFuel >> setUpPersistenceStrategyAndChecker [
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self actualPreferencesFileName.
	preferences ensureDelete.
	identifier persistenceStrategy: GlobalIdentifierPersistence fuel.
	identifier persistenceStrategy preferences: preferences.
	identifier persistenceStrategy checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).

]
