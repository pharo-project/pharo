storing
mergeStoredSettings: aCollectionOfStoredSettings
	^ StoredSettingsMerger new
		addStoredSettings: self allStoredSettings;
		addStoredSettings: aCollectionOfStoredSettings;
		storedSettings