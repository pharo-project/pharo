ancestry
breadthFirstAncestors
	^ Array streamContents: [:s | self breadthFirstAncestorsDo: [:ea | s nextPut: ea]]