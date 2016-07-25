accessing
persistence
	^ persistence ifNil: [ persistence := SystemSettingsPersistence settingTree: self ]