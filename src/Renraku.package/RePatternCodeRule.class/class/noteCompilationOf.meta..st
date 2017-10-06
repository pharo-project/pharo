testing
noteCompilationOf: aSelector meta: isMeta

	 aSelector == #initialize & isMeta not ifTrue: [ 
		ReRuleManager reset ]