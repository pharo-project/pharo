node access
hasNodeContents: node
	hasChildrenSelector ifNil:[^node contents isEmpty not].
	^model perform: hasChildrenSelector with: node item