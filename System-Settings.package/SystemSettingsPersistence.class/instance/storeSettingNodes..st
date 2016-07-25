storing
storeSettingNodes: aCollectionOfSettingNodes
	"It stores a collection of settings on the disk."
	| storedNodes |
	storedNodes := StoredSettingsFactory new fromSettingNodes: aCollectionOfSettingNodes.
	self storeStoredSettings: storedNodes.