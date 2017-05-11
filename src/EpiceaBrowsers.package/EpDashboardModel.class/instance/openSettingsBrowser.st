actions
openSettingsBrowser
	"Inspired on Glamour's code"

	| settingsBrowser settingsWindow |
	settingsBrowser := SettingBrowser new 
		changePackageSet: (RPackage organizer packageNamed: 'EpiceaBrowsers') asOrderedCollection;
		yourself.
	settingsWindow := settingsBrowser open.
	settingsWindow position: Display extent - settingsWindow extent // 2.
	settingsBrowser expandAll.