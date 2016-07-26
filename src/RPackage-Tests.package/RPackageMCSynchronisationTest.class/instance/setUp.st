setup
setUp 
	super setUp.
	
	emptyOrganizer := self organizer debuggingName: 'MCSynchronisation Package Organizer'; yourself.
	emptyOrganizer registerPackage: (self packageClass named: 'as yet unclassified').

	Author fullName 
		ifNil: [ Author fullName: 'Tester' ].