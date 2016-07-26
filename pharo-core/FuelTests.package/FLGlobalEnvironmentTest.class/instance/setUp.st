running
setUp 
	super setUp.
	serializationEnvironment := Dictionary new.
	materializationEnvironment := Dictionary new.
	
	self analyzer globalEnvironment: serializationEnvironment.
	self materializer globalEnvironment: materializationEnvironment.