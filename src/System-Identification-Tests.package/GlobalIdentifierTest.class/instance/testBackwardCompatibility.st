tests
testBackwardCompatibility
	"Let's say that we have information stored using FUEL, but we use STON.
	We do not have any STON stored value.
	Let's check that we load the FUEL, then stores it as STON."
	| fuelPersistence fuelPrefereces fuelDictionary  |
	fuelPersistence := GlobalIdentifierPersistence fuel.
	fuelPersistence checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).
	fuelPrefereces := preferences parent / 'old-preferences.fuel'.
	fuelPersistence preferences: fuelPrefereces.
	fuelDictionary := Dictionary new.
	fuelDictionary at: #computerUUID put: 42.
	fuelPersistence save: fuelDictionary.
	identifier persistenceStrategy previous: fuelPersistence.
	self deny: identifier computerUUID = 42.
	self assert: fuelPersistence preferences exists.
	identifier ensure.
	self assert: identifier computerUUID equals: 42 asString.
	self assert: identifier persistenceStrategy preferences exists.