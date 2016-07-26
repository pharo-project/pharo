coverage
selectorsToBeIgnored

	| deprecated private special |

	deprecated := #().
	private := #( #printOn: ).
	special := #( #next #do: ).

	^ super selectorsToBeIgnored, deprecated, private, special.