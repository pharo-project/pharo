accessing
childNamed: aName 
	
	| basicEntry childName |
	basicEntry := self lookupDirectory: name filename: aName.
	childName := name, self class pathDelimiter asString, aName.
	basicEntry ifNil: [ (FileDoesNotExistException fileName: childName) signal ].
	
	^ self childForEntry: basicEntry