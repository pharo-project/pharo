removing
removeBreakpointsFromMethod: aMethod 
	AllBreakpoints copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ AllBreakpoints remove: breakpoint ] ]