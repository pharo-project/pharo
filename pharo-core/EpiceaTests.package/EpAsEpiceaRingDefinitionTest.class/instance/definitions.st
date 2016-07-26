running
definitions
	"FIX: it should not have this external dependency to Fuel code entities."

	^ {	(FLSerializer >> #serialize:on:).
		(Object >> #fuelAccept:).
		FLSerializer.
		FLSerializer class. 
		FLTGlobalClassOrTraitSerializationTest.
		FLTGlobalClassOrTraitSerializationTest classTrait.
		} collect: #asEpiceaRingDefinition