as yet unclassified
assertVersionInfoPresent
	| dict info |
	dict := MczInstaller versionInfo at: self mockPackage name.
	info := expected info.
	self assertDict: dict matchesInfo: info.