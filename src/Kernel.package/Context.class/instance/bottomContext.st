query
bottomContext
	"Return the last context (the first context invoked) in my sender chain"

	^ self findContextSuchThat: [ :context | context sender isNil]