storing
storeVersion: aVersion
	self basicStoreVersion: aVersion.
	SystemAnnouncer uniqueInstance
		announce: (MCVersionSaved version: aVersion repository: self)