private
addAllItem: aSet selector: selector
	| set newSet arrayString |
	set := (self manifest perform: selector) asOrderedCollection.
	newSet := aSet \ (self unwrap: set).
	newSet isEmpty
		ifTrue: [ ^ self ].
	set addAll:
			(newSet
				collect: [ :fp | 
					{(fp asRingDefinition arrayStringForManifest).
					(DateAndTime current asString)} ]).
	arrayString := self buildArrayString: set.
	self compileSelector: selector returnValue: arrayString