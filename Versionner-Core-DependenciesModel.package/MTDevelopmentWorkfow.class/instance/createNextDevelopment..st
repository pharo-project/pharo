protocol
createNextDevelopment: newDevVersionString
	"Create the next baseline that will be used as development version."

	self createBaseline: newDevVersionString.
	self
		developmentVersion: newDevVersionString
		platformAttribute: #common.