instance creation
globalName: aName localName: anIdentifier

	^ self new
		initializeWithLocalName: anIdentifier
		globalName: aName;
		yourself