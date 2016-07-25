enumerating
settingNodeForStoredSetting: aStoredSetting
	^ self settingTree nodeList detect: [ :eachSettingNode |
		aStoredSetting isForSettingNode: eachSettingNode ]