accessing
viewedLabelOfSetting: aSettingDeclaration 
	| node |
	node := self settingNodeOfDeclaration: aSettingDeclaration.
	^ node 
		ifNil: [aSettingDeclaration label]
		ifNotNil: [(node allChildren notEmpty and: [aSettingDeclaration hasValue])
				ifTrue: [aSettingDeclaration label, '...'] 
				ifFalse: [aSettingDeclaration label]]