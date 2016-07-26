updating
openPathFrom: aSetting
	(self settingNodeOfDeclaration: aSetting) ifNotNil: [:node |
		self expandNodePath: node path]