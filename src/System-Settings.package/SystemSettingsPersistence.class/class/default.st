accessing
default
	^ self new
		fileReference: self defaultPreferenceFileReference;
		settingTree: SettingBrowser new treeHolder;
		yourself