public
directoryAt: aPath ifAbsent: absentBlock nodesDo: aBlock
	^ self
		nodeAt: aPath
		ifPresent: [ :entry | 
			(self basicIsDirectory: entry)
				ifTrue: [ self basicEntry: entry path: aPath nodesDo: aBlock ]
				ifFalse: [ DirectoryDoesNotExist signalWith: aPath ] ]
		ifAbsent: absentBlock