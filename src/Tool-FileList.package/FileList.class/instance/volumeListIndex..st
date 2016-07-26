volume list and pattern
volumeListIndex: index
	"Select the volume name having the given index."

	| delim path |
	volumeListIndex := index.
	index = 1 
		ifTrue: [self directory: FileSystem disk root ]
		ifFalse: [
			delim := reference fileSystem delimiter.
			path := String streamContents: [:stream |
				2 to: index do: [:i |
					stream nextPutAll: (volumeList at: i) trimBoth.
					i < index ifTrue: [stream nextPut: delim]]]].
	brevityState := #FileList.
	self addPath: path.
	self changed: #fileList.
	self changed: #contents.
	self updateButtonRow