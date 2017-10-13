private
childForEntry: anEntry
	
	| childName |
	childName := name, self class pathDelimiter asString, (anEntry at: 1).
	^ (anEntry at: 4) "is directory?"
		ifTrue: [ Directory named: childName ]
		ifFalse: [ File named: childName ]