coverage
selectorsToBeIgnored

	| deprecated private special |
	deprecated := #().
	private := #( #printOn: ).
	special := #( #next ).

	^ super selectorsToBeIgnored, deprecated, private, special.