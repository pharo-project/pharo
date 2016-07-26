ci
prepareNewUpdateSilently
	"Return nil on error"

	self launchUpdateSilently ifFalse: [ ^ nil ].
	[ self loadLatestScriptloader ]
		on: Warning
		do: [:warning | ^ nil ].
	self markPackagesBeforeNewCodeIsLoaded.
	self checkImageIsUptodateSilently ifFalse: [ self loadLatestUpdateListSilently ].
	self checkImageIsUptodate ifFalse: [ ^ nil ].
	self setUpdateAndScriptVersionNumbers.
	self saveAsNewImageWithCurrentReleaseName.
	^ self currentUpdateVersionNumber