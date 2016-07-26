*ston-core
stonOn: stonWriter
	self class == STON associationClass
		ifTrue: [ stonWriter writeAssociation: self ]
		ifFalse: [ super stonOn: stonWriter ]
	