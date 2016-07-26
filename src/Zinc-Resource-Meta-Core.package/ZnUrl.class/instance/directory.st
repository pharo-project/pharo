accessing-path
directory
	^ self isDirectoryPath
		ifTrue: [ self path ]
		ifFalse: [ 
			String streamContents: [ :stream |
				segments allButLast  
					do: [ :each | stream nextPutAll: each ]
					separatedBy: [ stream nextPut: $/ ] ] ]