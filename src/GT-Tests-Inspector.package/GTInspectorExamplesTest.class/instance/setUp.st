running
setUp
	super setUp.
	
	pragmas := IdentityDictionary new.
	GTExampleOrganizer instance reset.
	GTExampleOrganizer stop.