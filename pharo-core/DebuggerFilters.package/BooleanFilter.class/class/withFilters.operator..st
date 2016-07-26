instance creation
withFilters: someFilters operator: booleanOperator
	^self new 
		filters: someFilters; 
		booleanOperator: booleanOperator