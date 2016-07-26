backward compatibility
categoryOfElement: aSelector ifAbsent: aBlock

	^ (self protocolOrganizer protocolsOfSelector: aSelector)
		ifEmpty: [ 
			(organizedClass includesSelector: aSelector)
				ifTrue: [ Protocol unclassified ]
				ifFalse: aBlock ]
		ifNotEmpty: [:col | col first name ]