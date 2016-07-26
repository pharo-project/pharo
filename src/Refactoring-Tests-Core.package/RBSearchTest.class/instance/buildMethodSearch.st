accessing
buildMethodSearch
	"Simple forwarders"
	self 
		createMethodSearchWith: '`@methodName: `@args ^`@object `@methodName: `@args'
		selectors: #(#problemCount #isEmpty )
		inClass: RBTransformationRuleTest