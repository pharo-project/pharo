tests
testBackwardCompatibility4
	"Let's say that we have information stored using FUEL but we cannot read the file. 
	We use STON. The STON preference file is broken, we cannot read it.
	Let's check that we keep FUEL file untouched and creates STON file."
	| fuelPersistence fuelPrefereces data dictionary |
	fuelPersistence := GlobalIdentifierPersistence fuel.
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
	self deny: identifier persistenceStrategy preferences contents = data.
	dictionary := Dictionary new.
	identifier persistenceStrategy load: dictionary.
	self assert: dictionary equals: identifier persistedInformation.