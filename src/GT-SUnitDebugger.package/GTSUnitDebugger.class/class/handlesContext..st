testing
handlesContext: aContext
	| activationPredicate |
	activationPredicate := self sessionClass activationPredicate.
	^ (aContext stackOfSize: self lookupStackSize) 
		anySatisfy: [ :ctx | activationPredicate matches: ctx ] 