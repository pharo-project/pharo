deleting
deleteSettingNodes: aCollection 
	| storedSettings reducedStoredSettings |
	storedSettings := self storedSettingsForSettingNodes: aCollection.
	reducedStoredSettings := self allStoredSettings difference: storedSettings . 
	self storeExactStoredSettings: reducedStoredSettings