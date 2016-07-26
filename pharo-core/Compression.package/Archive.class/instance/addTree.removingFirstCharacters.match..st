archive operations
addTree: aFileNameOrDirectory removingFirstCharacters: n match: aBlock
	| dir relativePath matches |
	dir := aFileNameOrDirectory asFileReference.
	relativePath := (dir fullName allButFirst: n) asFileReference.
	matches := dir entries select: [ :entry | aBlock value: entry ].
	matches
		do: [ :e | | newMember |
			newMember := e isDirectory
					ifTrue: [ self memberClass newFromDirectory: e fullName ]
					ifFalse: [ self memberClass newFromFile: e fullName ].
		newMember localFileName: (relativePath / e name) fullName.
		self addMember: newMember.
		e isDirectory ifTrue: [ self addTree: e fullName removingFirstCharacters: n match: aBlock ].
	].
