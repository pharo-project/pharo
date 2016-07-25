tests
testOpening
	"Non-Regression Test related to https://pharo.fogbugz.com/f/cases/16138/WorldMenu-System-Settings-throws-DNU-Image-50217"
	| settingBrowser |
	self
		shouldnt: [ 
			settingBrowser := SettingBrowser open.
			settingBrowser close ]
		raise: Error