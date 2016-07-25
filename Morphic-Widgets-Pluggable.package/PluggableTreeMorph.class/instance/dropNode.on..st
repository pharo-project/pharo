node access
dropNode: srcNode on: dstNode
	dropItemSelector ifNil:[^nil].
	model perform: dropItemSelector with: srcNode item with: dstNode item