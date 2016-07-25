filtering
keepSettingTreeNode: aSettingNode 
	| rp |
	^ (rp := aSettingNode receiverPackage) notNil and: [self packages includes: rp]