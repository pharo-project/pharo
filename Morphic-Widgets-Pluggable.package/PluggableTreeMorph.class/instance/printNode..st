node access
printNode: node
	getLabelSelector ifNil:[^node item printString].
	^model perform: getLabelSelector with: node item