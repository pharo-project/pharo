libraries
lookup: symbol "<String>" inLibrary: libraryNameOrAlien "<String|Alien> ^<Alien>"
	^self forPointer: ((libraryNameOrAlien isString
						ifTrue: [self ensureLoaded: libraryNameOrAlien]
						ifFalse: [libraryNameOrAlien])
							primFindSymbol: symbol)