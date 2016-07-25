private
haltIfCallChainContains: aSelector
	| cntxt |
	cntxt := thisContext.
	[ cntxt sender isNil ]
		whileFalse: [ cntxt := cntxt sender.
			cntxt selector = aSelector
				ifTrue: [ self signal ] ]