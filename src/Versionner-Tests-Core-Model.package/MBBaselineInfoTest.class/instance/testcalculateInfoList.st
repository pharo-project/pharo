tests
testcalculateInfoList

	"Doesn't work!
	 Need to use a real configuration with all information "
	"info := (MBBaselineInfo version: (MBVersionInfo version: (MetacelloMCVersion fromString: '1.0-baseline')))
		configurationInfo: (MBConfigurationInfo configurationClass: configuration).
		
	info calculateInfoList"