category
removeEmptyUnclassifiedCategoryFrom: aClass
	"Remove the 'as yet unclassified' category from the class if empty"

	| protocolOrganizer |
	protocolOrganizer := aClass organization protocolOrganizer.
	protocolOrganizer protocols
		detect: [ :protocol | protocol name = Protocol unclassified and: [ protocol isEmpty ] ]
		ifFound: [ :unclassifiedProtocol | protocolOrganizer removeProtocol: unclassifiedProtocol ]