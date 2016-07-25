enumerating
storedSettingForSettingNode: aSettingNode
	"It returns a stored setting for the setting node."
	^ self allStoredSettings 
				detect: [ :eachStoredSetting | 
					eachStoredSetting isForSettingNode: aSettingNode ]
				ifNone: [ nil ]