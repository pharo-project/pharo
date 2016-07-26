private
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem
        | sourceNode destinationNode |

        sourceNode := self
                nodeAt: sourcePath
                ifPresent: [ :source | source ]
                ifAbsent: [ ^ absentBlock value].

        sourceNode isDirectory
                ifTrue: [ ^ absentBlock value ].

        destinationNode := self
                nodeAt: destinationPath parent
                ifPresent: [ :destination |  destination ]
                ifAbsent: [ ^ self signalDirectoryDoesNotExist: destinationPath parent ].

        destinationNode isFile
                ifTrue: [ self signalDirectoryDoesNotExist: destinationPath parent ].

        (destinationNode fileEntriesIncludes: destinationPath basename)
                ifTrue: [ "cannot overwrite existing file"^ presentBlock value ].

        destinationNode
                fileEntryAt: destinationPath basename
                put: (sourceNode copy
                                        basename: destinationPath basename;
                                        yourself)
