testing
testStWriter
	"self debug: #testStWriter"
	self
		assertFileOutFrom: MCStWriter
		canBeFiledInWith: [ CodeImporter evaluateReadStream: stream readStream].
