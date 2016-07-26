utilities
definitionFromFile: directoryEntry inDirectory: aDirectory
    | defs reader |
    directoryEntry ifNil: [ ^ nil ].
    self fileUtils
        readStreamFor: directoryEntry name
        in: aDirectory
        do: [ :fileStream |
            reader := MCStReader on: fileStream.
            defs := reader definitions.
		defs size <= 1
                ifFalse: [ self error: 'we should be writing exactly 1 definition per file' ] ].
    defs size = 0
        ifTrue: [ ^ nil ].
    ^ defs first