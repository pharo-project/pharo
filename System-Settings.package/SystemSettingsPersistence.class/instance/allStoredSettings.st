accessing
allStoredSettings
	"Load all stored settings from the file preference."
	(self fileReference exists) 
		ifFalse: [ ^ #() ].
	^ SettingsStonReader new stream: self readStream; secureLoad.