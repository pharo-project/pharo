morphic ui
repositorySearchAccept: string
	| aString |

	aString := string ifNil: [ '' ].
	repositoryPattern = aString asLowercase ifTrue: [ ^ self ].
	repositoryPattern := aString asLowercase.
		
	repositoryProcess ifNotNil: [ repositoryProcess terminate ].
	repositoryProcess := [ self repositoryListChanged ] fork.