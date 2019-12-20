"
A GlobalIdentifierTest is a test class for testing the behavior of GlobalIdentifier
"
Class {
	#name : #GlobalIdentifierTest,
	#superclass : #TestCase,
	#instVars : [
		'identifier',
		'preferences'
	],
	#category : #'System-Identification-Tests'
}

{ #category : #testing }
GlobalIdentifierTest class >> isAbstract [
	^ self = GlobalIdentifierTest 
]

{ #category : #accessing }
GlobalIdentifierTest >> actualPreferencesFileName [
	^ identifier persistenceStrategy fileName
]

{ #category : #accessing }
GlobalIdentifierTest >> folderName [
	^ 'pharoPreferencesTestFolder'
]

{ #category : #'instance creation' }
GlobalIdentifierTest >> newChecker [
	^ GlobalIdentifierPersistenceMockChecker new
]

{ #category : #running }
GlobalIdentifierTest >> setUp [
	super setUp.
	identifier := GlobalIdentifier basicNew initialize.
	self setUpPersistenceStrategyAndChecker
]

{ #category : #running }
GlobalIdentifierTest >> setUpPersistenceStrategyAndChecker [
	self subclassResponsibility
]

{ #category : #running }
GlobalIdentifierTest >> tearDown [
	preferences ensureDelete.
	(FileLocator workingDirectory / self folderName) 
		ensureDeleteAllChildren; 
		ensureDelete.
	super tearDown.	
]

{ #category : #tests }
GlobalIdentifierTest >> testBackwardCompatibility [
	"Let's say that we have information stored using FUEL, but we use STON.
	We do not have any STON stored value.
	Let's check that we load the FUEL, then stores it as STON."

	| fuelPersistence fuelPrefereces fuelDictionary |
	fuelPersistence := GlobalIdentifierPersistence fuel.
	fuelPersistence
		checker:
			(self newChecker
				answer: [ FileLocator workingDirectory exists ];
				yourself).
	fuelPrefereces := preferences parent / 'old-preferences.fuel'.
	fuelPersistence preferences: fuelPrefereces.
	fuelDictionary := Dictionary new.
	fuelDictionary at: #computerUUID put: 42.
	fuelPersistence save: fuelDictionary.
	identifier persistenceStrategy previous: fuelPersistence.
	self deny: identifier computerUUID equals: 42.
	self assert: fuelPersistence preferences exists.
	identifier ensure.
	self assert: identifier computerUUID equals: 42 asString.
	self assert: identifier persistenceStrategy preferences exists
]

{ #category : #tests }
GlobalIdentifierTest >> testBackwardCompatibility2 [
	"Let's say that we have information stored using both FUEL and STON, and we use STON.
	We will ingnore FUEL file and load STON."

	| fuelPersistence fuelPrefereces fuelDictionary stonDictionary |
	fuelPersistence := GlobalIdentifierPersistence fuel.
	fuelPersistence
		checker:
			(self newChecker
				answer: [ FileLocator workingDirectory exists ];
				yourself).
	fuelPrefereces := preferences parent / 'old-preferences.fuel'.
	fuelPersistence preferences: fuelPrefereces.
	fuelDictionary := Dictionary new.
	fuelDictionary at: #computerUUID put: 42.
	fuelPersistence save: fuelDictionary.
	identifier persistenceStrategy previous: fuelPersistence.
	stonDictionary := Dictionary new.
	stonDictionary at: #computerUUID put: 21.
	identifier persistenceStrategy save: stonDictionary.
	self deny: identifier computerUUID equals: 42.
	self deny: identifier computerUUID equals: 21.
	self assert: fuelPersistence preferences exists.
	self assert: identifier persistenceStrategy preferences exists.
	identifier ensure.
	self assert: identifier computerUUID equals: 21 asString
]

{ #category : #tests }
GlobalIdentifierTest >> testBackwardCompatibility3 [
	"Let's say that we have information stored using FUEL but we cannot read the file. 
	We use STON. We do not have any STON stored value.
	Let's check that we keep FUEL file untouched and creates STON file."
	| fuelPersistence fuelPrefereces data |
	fuelPersistence := GlobalIdentifierPersistence fuel.
	fuelPersistence checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).
	fuelPrefereces := preferences parent / 'old-preferences.fuel'.
	fuelPrefereces parent ensureCreateDirectory.
	data := 'some data'.
	fuelPrefereces writeStreamDo: [ :aStream | aStream nextPutAll: data ].
	fuelPersistence preferences: fuelPrefereces.
	identifier persistenceStrategy previous: fuelPersistence.
	self assert: fuelPrefereces exists.
	self assert: fuelPrefereces contents equals: data.
	self deny: identifier persistenceStrategy preferences exists.
	identifier ensure.
	self assert: fuelPrefereces exists.
	self assert: fuelPrefereces contents equals: data.
	self assert: identifier persistenceStrategy preferences exists.
]

{ #category : #tests }
GlobalIdentifierTest >> testBackwardCompatibility4 [
	"Let's say that we have information stored using FUEL but we cannot read the file. 
	We use STON. The STON preference file is broken, we cannot read it.
	Let's check that we keep FUEL file untouched and creates STON file."

	| fuelPersistence fuelPrefereces data dictionary |
	fuelPersistence := GlobalIdentifierPersistence fuel.
	fuelPersistence
		checker:
			(self newChecker
				answer: [ FileLocator workingDirectory exists ];
				yourself).
	fuelPrefereces := preferences parent / 'old-preferences.fuel'.
	fuelPrefereces parent ensureCreateDirectory.
	data := 'some data'.
	"we create unreadable file."
	fuelPrefereces writeStreamDo: [ :aStream | aStream nextPutAll: data ].
	"we create broken file."
	identifier persistenceStrategy preferences writeStreamDo: [ :aStream | aStream nextPutAll: data ].
	fuelPersistence preferences: fuelPrefereces.
	identifier persistenceStrategy previous: fuelPersistence.
	self assert: fuelPrefereces exists.
	self assert: fuelPrefereces contents equals: data.
	self assert: identifier persistenceStrategy preferences exists.
	self assert: identifier persistenceStrategy preferences contents equals: data.
	identifier ensure.
	self assert: fuelPrefereces exists.
	self assert: fuelPrefereces contents equals: data.
	self assert: identifier persistenceStrategy preferences exists.
	self deny: identifier persistenceStrategy preferencesContents equals: data.
	dictionary := Dictionary new.
	identifier persistenceStrategy load: dictionary.
	self assert: dictionary equals: identifier persistedInformation
]

{ #category : #tests }
GlobalIdentifierTest >> testComputerUUID [
	"The computer UUID is not stored immediately because the Pharo Integration Server
	raised building errors when storing the computer UUID on the CI server."
	self assert: identifier computerUUID notNil.
	self assert: identifier computerUUID isString.
	self testDefault.
]

{ #category : #tests }
GlobalIdentifierTest >> testDefault [
	self deny: preferences exists description: 'The preference file should not exists by default.'.
	self deny: preferences parent exists description: 'The preference directory should not exists by default.'
]

{ #category : #tests }
GlobalIdentifierTest >> testDoNotStoreIfHomeDoesNotExists [
	"Ignore persistence if HOME directory does not exists.
	In the case that the image is deployed on a server, HOME directory may not exists.
	See issue https://pharo.manuscript.com/f/cases/19944."
	| strategy |
	strategy := identifier persistenceStrategy.
	[ strategy ] whileNotNil: [ 
		strategy checker: (self newChecker answer: false; yourself).
		strategy := strategy previous.
	].
	identifier ensure.
	strategy := identifier persistenceStrategy.
	[ strategy ] whileNotNil: [ 
		self deny: strategy preferences exists.
		strategy := strategy previous.
	].
	self assert: identifier computerUUID notNil.

]

{ #category : #tests }
GlobalIdentifierTest >> testEnsure [
	"The computer UUID is not stored immediately because the Pharo Integration Server
	raised building errors when storing the computer UUID on the CI server."
	| stored |
	self testPreferenceFilesNotCollideWithRealPreferences.
	identifier ensure.
	self assert: preferences exists description: 'The actual preferences should be stored now.'.
	stored := identifier persistenceStrategy load.
	self assert: stored equals: identifier persistedInformation.

]

{ #category : #tests }
GlobalIdentifierTest >> testPreferenceFilesNotCollideWithRealPreferences [
	"Ensure that setUp is done correctly and we do not override or change production preferences.
	See https://pharo.manuscript.com/f/cases/19501"
	| strategy |
	strategy := identifier persistenceStrategy.
	[ strategy ] whileNotNil: [ 
		self deny: (FileLocator preferences contains: strategy preferences)
			description: 'Tested preference file should NOT be stored in the preferences directory.'.
		self assert: (FileLocator preferences contains: strategy defaultPreferences)
			description: 'Default preference file should BE stored in the preferences directory.'.
		strategy := strategy previous.
	].
]
