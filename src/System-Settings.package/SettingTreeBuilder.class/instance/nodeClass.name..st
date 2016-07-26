private tree building
nodeClass: aClass name: aSymbol
	| node |
	node := self nodeClass with: aClass new.
	node item name: aSymbol.
	node pragma: currentPragma.
	node parentName: (currentParent ifNotNil: [currentParent name]).
	self nodeList add: node.
	^ (SettingNodeBuilder new) node: node; builder: self; yourself