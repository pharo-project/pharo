coverage
selectorsToBeIgnored

	 | deprecated private special |
	deprecated := #().
	private := #().
	special := #( #< #= #new #next #previous #printOn: #printOn:format: #storeOn: #fromString: ).

	^ super selectorsToBeIgnored, deprecated, private, special