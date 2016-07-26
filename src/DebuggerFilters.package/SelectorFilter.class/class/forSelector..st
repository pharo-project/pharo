instance creation
forSelector: aSelector
	|filter|
	
	filter:= self new.
	filter selectors add: aSelector.
	^ filter