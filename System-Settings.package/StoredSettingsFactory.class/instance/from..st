building
from: aSettingNode 
	storedSetting := nil.
	aSettingNode item acceptSettings: self. 
	storedSetting ifNotNil: [ 
		storedSetting settingNodeIdentifier: aSettingNode settingNodeIdentifier ].
	^ storedSetting