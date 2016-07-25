filtering
keepSettingTreeNode: aSettingNode 
	^ (({aSettingNode methodClass name. aSettingNode selector}
			collect: [:e | e ifNil: ['']])
			anySatisfy: [:token | token substrings anySatisfy: [:sub | sub matchesRegexIgnoringCase: self regexp]])
		or: [self keepSettingDeclaration: aSettingNode settingDeclaration] 