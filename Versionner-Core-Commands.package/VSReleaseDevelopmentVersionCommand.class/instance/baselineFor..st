actions
baselineFor: aVersion
	"Return the baseline on which this version relies on"
	| baselineVersionString |
	
	baselineVersionString := aVersion importedVersions at: 1.
	^ aVersion project version: baselineVersionString