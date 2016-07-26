storing
storeExactStoredSettings: allStoredSettings
	self removeFileReference.
	SettingsStonWriter new
		stream: self writeStream;
		addSettings: allStoredSettings;
		store