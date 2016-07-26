tests
testBackwardCompatibility3
	"Let's say that we have information stored using FUEL but we cannot read the file. 
	We use STON. We do not have any STON stored value.
	Let's check that we keep FUEL file untouched and creates STON file."
	| fuelPersistence fuelPrefereces data |
	fuelPersistence := GlobalIdentifierPersistence fuel.
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