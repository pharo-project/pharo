updating
changePackageSet: aSet
	"
	This method allow for restricting the settings displayed in the browser. 
	Opening a setting browser display all the setting. 
	SettingBrowser new changePackageSet: { (RPackageOrganizer default packageNamed: 'GT-Playground') }; open
	"
	aSet ~= self viewedPackages
		ifTrue: [self viewedPackages: aSet.
			self applyFilters].
	^ true