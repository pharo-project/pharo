storing
storeIdentifier: settingNodeIdentifierString
	"It stores one setting, identifier by settingNodeIdentifierString, on the disk."
	self storeIdentifiers: (Array with: settingNodeIdentifierString)