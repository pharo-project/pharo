loading
updateSettingNodes: aCollectionOfSettingNodes
	| storedSettings |
	storedSettings := self allStoredSettings.
	aCollectionOfSettingNodes do: [ :eachSettingNode |
		storedSettings 
			detect: [ :eachStoredSetting | eachStoredSetting isForSettingNode: eachSettingNode ]
			ifFound: [ :storedSetting | storedSetting updateSettingNode: eachSettingNode ]
			ifNone: [ "ignore it" ] ]