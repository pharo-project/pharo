public
value: aSpotterContext
	"I provide an entry point for performing a search that is 
	polymorphic with BlockClosure>>value:. I return the list of
	filtered items."
	
	^ [ [ self applyFilterInContext: aSpotterContext ]
		on: Error
		do: [ :exception | 
			self exceptionHandler 
				handleException: exception 
				do: [ self applyFilterInContext: aSpotterContext error: exception ] ] ] 
		ensure: [ context := nil " release the context after search is completed " ] 