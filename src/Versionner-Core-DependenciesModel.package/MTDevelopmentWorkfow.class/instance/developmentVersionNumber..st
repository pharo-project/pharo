private
developmentVersionNumber: aDevelopmentVersionString
	| versionNumber |
	versionNumber := aDevelopmentVersionString copyReplaceAll: 'baseline' with: ''.
	versionNumber  :=  versionNumber copyReplaceAll: '-' with: ''.
	^ versionNumber