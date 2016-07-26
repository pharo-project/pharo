enumerating
storedSettingsForSettingNodes: aCollectionOfSettingNodes
	"It returns stored settings for the setting nodes."
	^ self allStoredSettings 
				select: [ :eachStoredSetting | 
					aCollectionOfSettingNodes anySatisfy: [ :eachSettingNode |
						eachStoredSetting isForSettingNode: eachSettingNode ] ]