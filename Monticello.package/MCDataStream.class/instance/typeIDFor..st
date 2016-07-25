write and read
typeIDFor: anObject
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	^ self class typeMap at: anObject class ifAbsent: [9 "instance of any normal class"]	
"See DataStream initialize.  nil=1. true=2. false=3. a SmallInteger=4. (a String was 5). a Symbol=6.  a ByteArray=7. an Array=8. other = 9.  a Bitmap=11. a Metaclass=12. a Float=14.  a Rectangle=15. any instance that can have a short header=16.  a String=17 (new format). a WordArray=18."