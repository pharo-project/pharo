helpers
getStreamsInfoForRunningTest
	^ Array with: self numberOfOpenFiles with: self numberOfExistingTempStreamFiles 