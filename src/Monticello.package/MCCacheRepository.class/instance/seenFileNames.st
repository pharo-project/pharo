accessing
seenFileNames
	^ seenFiles ifNil: [seenFiles := OrderedCollection new]