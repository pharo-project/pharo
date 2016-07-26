scripts
releaseVersion: developmentVersionString for: configurationBasename
	"Set the #common blessing to #release and set the #common #stable symbolic version to the given version."

	<apiDocumentation>
	^ self
		releaseVersion: developmentVersionString
		for: configurationBasename
		blessingAttribute: #common
		platformAttribute: #common