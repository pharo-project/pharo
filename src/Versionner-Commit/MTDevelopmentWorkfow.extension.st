Extension { #name : #MTDevelopmentWorkfow }

{ #category : #'*Versionner-Commit' }
MTDevelopmentWorkfow >> releaseFixedDevelopment: versionName [
	"Release the development baseline (will give a version method) with the given version name."

	| devVersionString |
	devVersionString := self developmentVersion versionString.
	MetacelloToolBox
		createFixedVersion: versionName
		for: project name
		from: devVersionString 
		description: 'version ' , versionName.
	self setStableBlessingFor: versionName.
	MetacelloToolBox 
		stableVersion: versionName 
		for: project name
		platformAttribute: #common. 
	"keep the development version pointing to the baseline"	
	self
		developmentVersion: devVersionString
		platformAttribute: #common.
	self notifyConfigurationModified
]

{ #category : #'*Versionner-Commit' }
MTDevelopmentWorkfow >> releaseFixedDevelopment: versionName description: description [
	"Release the development baseline (will give a version method) with the given version name."

	| devVersionString |
	devVersionString := self developmentVersion versionString.
	MetacelloToolBox
		createFixedVersion: versionName
		for: project name
		from: devVersionString 
		description: (description ifNil: [ 'version ' , versionName]).
	self setStableBlessingFor: versionName.
	MetacelloToolBox 
		stableVersion: versionName 
		for: project name
		platformAttribute: #common. 
	"keep the development version pointing to the baseline"	
	self
		developmentVersion: devVersionString
		platformAttribute: #common.
	self notifyConfigurationModified
]
