ancestry
hasAncestor: aVersionInfo alreadySeen: aList
	(aList includes: self) ifTrue: [^ false].
	aList add: self.

	^ self = aVersionInfo or: [self ancestors anySatisfy: [:ea | ea hasAncestor: aVersionInfo alreadySeen: aList]]
