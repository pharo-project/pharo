accessing
settingNodeOfDeclaration: aSettingDeclaration
	^ self treeHolder deeplyDetect: [:n | n settingDeclaration = aSettingDeclaration]