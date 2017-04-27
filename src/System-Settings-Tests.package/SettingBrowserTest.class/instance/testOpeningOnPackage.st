tests
testOpeningOnPackage

	settingBrowser := SettingBrowser new changePackageSet: { (RPackageOrganizer default packageNamed: 'GT-Playground') }; open.
	settingBrowser close