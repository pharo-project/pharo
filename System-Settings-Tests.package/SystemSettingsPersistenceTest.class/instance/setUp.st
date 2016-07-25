running
setUp
	super setUp.
	MockSettings cleanUp.
	memory := FileSystem memory.
	preferences := memory root / 'pharo' / 'pharoX.Y' / 'preferences.ston.txt'.
	systemSettings := SystemSettingsPersistence new.
	systemSettings 
		fileReference: preferences;
		settingTree: MockSettings settingTree.
	