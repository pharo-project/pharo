updating
childrenDo: aBlock

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aBlock value: aNode].
	]