filtering
keepSettingTreeNode: aSettingNode 
	^ (({aSettingNode methodClass name asUppercase. aSettingNode selector asUppercase}
			collect: [:e | e ifNil: ['']])
			includesSubstringAnywhere: self token asUppercase)
		or: [self keepSettingDeclaration: aSettingNode settingDeclaration]