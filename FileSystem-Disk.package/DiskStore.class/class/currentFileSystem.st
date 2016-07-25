current
currentFileSystem
	^ CurrentFS ifNil: [
		CurrentFS := FileSystem store: self activeClass createDefault]