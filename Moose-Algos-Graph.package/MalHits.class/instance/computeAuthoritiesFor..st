running
computeAuthoritiesFor: aNode

	aNode auth: (aNode previousNodes inject: 0 into: [:s :n| s + n hub])