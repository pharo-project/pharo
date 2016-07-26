instance creation
forSelectors: someSelectors
	 |filter|
	
	 filter:= self new.
	 filter selectors addAll: someSelectors.
	^ filter