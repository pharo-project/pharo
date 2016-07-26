accessing
recordingSpecLoader

	^(MetacelloNullRecordingMCSpecLoader on: self spec)
		shouldDisablePackageCache: self shouldDisablePackageCache;
		loaderPolicy: self loaderPolicy copy;
		yourself