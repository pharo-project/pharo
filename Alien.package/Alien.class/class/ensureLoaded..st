libraries
ensureLoaded: aLibraryName "<String> ^<Alien>"
	^LoadedLibraries
		at: aLibraryName
		ifAbsentPut:
			[| lib alien |
			 lib := self primLoadLibrary: aLibraryName.
			 alien := Alien new: WordSize.
			 WordSize = 4
				ifTrue: [alien unsignedLongAt: 1 put: lib]
				ifFalse: [alien unsignedLongLongAt: 1 put: lib].
			 alien]