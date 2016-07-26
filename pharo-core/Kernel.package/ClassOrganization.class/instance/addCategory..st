accessing
addCategory: aString
	| oldCategories |
	
	(self protocolOrganizer hasProtocolNamed: aString)
		ifTrue: [ ^self ].

	oldCategories := self categories copy.
	self protocolOrganizer addProtocolNamed: aString.
	self notifyOfAddedCategory: aString.
	self 
		notifyOfChangedCategoriesFrom: oldCategories 
		to: self categories.