*Iceberg-Core
changesFileNamed: fileName
	| lookup myVersion parentVersions |
	lookup := [:commit | [ commit entryByPath: fileName ] on: Error do: [ nil ]].
	myVersion := lookup value: self.
	parentVersions := self parents collect: lookup.
	
	^ (myVersion 
		ifNil: [ parentVersions includes: nil ]
		ifNotNil: [ parentVersions anySatisfy: [ :version | 
				version notNil and: [ version objectId = myVersion objectId ]]]) not
