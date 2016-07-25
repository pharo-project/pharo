updating
applyFilters
	roots := nil.
	self searchedText
		ifNotEmpty: [self updateAccordingTo: self textFilter].
	self viewedPackages
		ifNotEmpty: [ | filter |
			filter := SettingPackageFilter new
			packages: self viewedPackages.
			self updateAccordingTo: filter].
	self updateList
