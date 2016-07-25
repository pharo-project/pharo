node access
iconOfNode: node
	getIconSelector ifNil:[^nil].
	^model perform: getIconSelector with: node item