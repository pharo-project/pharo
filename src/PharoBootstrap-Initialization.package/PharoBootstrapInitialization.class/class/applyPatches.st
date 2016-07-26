temporary-patch
applyPatches

	self
		"fixShutdownList;"
		"fixOSPlatformInitialization;"
		"fixEndianDetectorBecomeCompact;"
		fixSetInitialCodeFormatter;
		fixUpdateInstancesOfSpecialClasses;
		fixPharoClassInstallerMethodCopying";
		fixCreateDisplayScreen"