node access
wantsDroppedNode: srcNode on: dstNode
	dropItemSelector ifNil:[^false].
	wantsDropSelector ifNil:[^true].
	^(model perform: wantsDropSelector with: srcNode with: dstNode) == true