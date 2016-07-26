storing
storeStoredSettings: aCollectionOfStoredSettings
	"It stores a collection of settings on the disk."
	| allStoredSettings |
	allStoredSettings := self mergeStoredSettings: aCollectionOfStoredSettings.
	self storeExactStoredSettings: allStoredSettings