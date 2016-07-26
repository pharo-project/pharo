updating
recursiveAddTo: aCollection

	firstChild ifNotNil: [firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: aCollection]].
	aCollection add: self.

	