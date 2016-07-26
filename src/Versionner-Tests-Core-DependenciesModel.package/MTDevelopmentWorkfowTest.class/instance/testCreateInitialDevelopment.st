tests
testCreateInitialDevelopment
	|  version configuration |

	project := MTProject newNamed: 'Z' withInitialVersion: '0.1'  inRepository: 'http://smalltalkhub.com/mc/demarey/Versionner/main'.
	configuration := project configurationClass.
	
	visitor := MCModel2MTModelVisitor new.
	self shouldnt: [ version := configuration project version: #development]
		 raise: MetacelloSymbolicVersionDoesNotExistError
		description: 'Cannot find the #development symbolic version in the metacello description'.
	
	self assert: configuration project development versionString
		equals: '0.1-baseline'.
	
	"configuration removeFromSystem."