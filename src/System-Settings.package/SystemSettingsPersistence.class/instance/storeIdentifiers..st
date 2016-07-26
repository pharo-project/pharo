storing
storeIdentifiers: aCollectionOfSettingNodeIdentifiers
	"It stores one setting, identifier by settingNodeIdentifierString, on the disk."
	| settingNodes |
	settingNodes := self settingsNodesFromIdentifiers: aCollectionOfSettingNodeIdentifiers.
	self storeSettingNodes: settingNodes