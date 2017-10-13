tests
testLog
	| critiques method |
	method := thisContext method.

	ReExceptionStrategy current: ReLogExceptionStrategy.
	
	self shouldnt: [
		critiques := method critiques ]
		raise: MyTestError.
		
	self assert: (critiques anySatisfy: [ :crit | 
		(crit isKindOf: ReExceptionProperty) and: [
		 crit sourceAnchor entity = method and: [
		(crit message includesSubstring: self errorMessage) and: [
		 crit stack callChainAnySatisfy: [ :context |
			context method = method ] ] ] ] ])