querying
isInstalledIn: aMethod

	AllBreakpoints do: [ :breakpoint |
		breakpoint link methods 
			detect: [ :m | m == aMethod ]
			ifFound: [ ^ true ]
	].

	^ false