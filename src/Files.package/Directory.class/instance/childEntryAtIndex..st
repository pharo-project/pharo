private
childEntryAtIndex: anInteger 
	
	| basicEntry childName |
	basicEntry :=  self lookupEntryIn: name index: anInteger.
	basicEntry ifNil: [ ^ nil ].
	childName := name, self class pathDelimiter asString, (basicEntry at: 1).
	^ (basicEntry at: 4) "is directory?"
		ifTrue: [ Directory named: childName ]
		ifFalse: [ File named: childName ]