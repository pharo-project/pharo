tests
testBackwardCompatibility2
	"Let's say that we have information stored using both FUEL and STON, and we use STON.
	We will ingnore FUEL file and load STON."
	| fuelPersistence fuelPrefereces fuelDictionary  stonDictionary |
	fuelPersistence := GlobalIdentifierPersistence fuel.
	fuelPrefereces := preferences parent / 'old-preferences.fuel'.
	fuelPersistence preferences: fuelPrefereces.
	fuelDictionary := Dictionary new.
	fuelDictionary at: #computerUUID put: 42.
	fuelPersistence save: fuelDictionary.
	identifier persistenceStrategy previous: fuelPersistence.
	stonDictionary := Dictionary new.
	stonDictionary at: #computerUUID put: 21.
	identifier persistenceStrategy save: stonDictionary.
	self deny: identifier computerUUID = 42.
	self deny: identifier computerUUID = 21.
	self assert: fuelPersistence preferences exists.
	self assert: identifier persistenceStrategy preferences exists.
	identifier ensure.
	self assert: identifier computerUUID equals: 21 asString.
