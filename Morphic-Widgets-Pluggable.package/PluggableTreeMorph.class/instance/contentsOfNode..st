node access
contentsOfNode: node
	| children |
	getChildrenSelector ifNil:[^#()].
	children := model perform: getChildrenSelector with: node item.
	^children collect:[:item| PluggableTreeItemNode with: item model: self]