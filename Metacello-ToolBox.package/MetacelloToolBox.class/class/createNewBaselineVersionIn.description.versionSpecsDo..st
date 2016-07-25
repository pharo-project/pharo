development support
createNewBaselineVersionIn: aConfigurationClass description: creationMessage versionSpecsDo: versionSpecBlock
	"Create a new baseline version based upon #stable version's baseline. Evaluate <versionSpecBlock> for each section.
	 A new baseline should be created if new packages or projects have been added or package dependencies have changed."

	<apiDocumentation>
	| currentVersion existingBaselineVersionString baselineVersionNumber |
	currentVersion := aConfigurationClass project version: #stable.
	existingBaselineVersionString := currentVersion importedVersions first.
	baselineVersionNumber := existingBaselineVersionString asMetacelloVersionNumber incrementMinorVersionNumber.
	self
		createBaseline: baselineVersionNumber versionString
		for: aConfigurationClass name asString
		from: existingBaselineVersionString
		description: creationMessage
		versionSpecsDo: versionSpecBlock