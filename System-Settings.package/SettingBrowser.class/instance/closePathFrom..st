updating
closePathFrom: aSetting
	(self settingNodeOfDeclaration: aSetting)
		ifNotNil: [ :node | self requestView: (MorphTreeChangeRequest collapseNodePath: (node path collect: [ :n | n withoutListWrapper ])) ]