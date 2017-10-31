tests
testVersion
	"Check that the #version method returns without a $- between Pharo and the major number."

	| version |
	version := SystemVersion fromVersionString: 'Pharo-7.0+alpha.build.2.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.
	self assert: version version equals: 'Pharo7.0alpha'