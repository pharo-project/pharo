comparing
= aStoredSetting
	^ self class = aStoredSetting class and: [ 
		self settingNodeIdentifier = aStoredSetting settingNodeIdentifier ]