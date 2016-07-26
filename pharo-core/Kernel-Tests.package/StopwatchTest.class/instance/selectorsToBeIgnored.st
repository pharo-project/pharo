coverage
selectorsToBeIgnored

	| private | 
	private := #( #printOn: #state: ).

	^ super selectorsToBeIgnored, private
