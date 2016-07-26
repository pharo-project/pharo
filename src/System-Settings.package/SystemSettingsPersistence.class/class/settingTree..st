instance creation
settingTree: aSettingTree 
	^ self new
		fileReference: self defaultPreferenceFileReference;
		settingTree: aSettingTree;
		yourself