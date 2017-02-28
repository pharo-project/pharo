lookup
nextOuterScopeContextOf: aContext

	"Returns the next context to lookup a variable name from within outer scope.
	
	For inlined BlockClosure which I represent it will be same context because inlined blocks do not produce extra contexts on stack"
	^ aContext