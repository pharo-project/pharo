replacing
remove: aNode
	aNode sequence: nil.
	sequence remove: aNode ifAbsent: [self error].