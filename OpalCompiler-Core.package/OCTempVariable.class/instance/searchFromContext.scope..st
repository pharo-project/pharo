debugging
searchFromContext: aContext scope: contextScope
	"Usually the loop is useless, the first case with i = offset returns the correct result. 
	However, the loop is required for specific case as Debugger restart feature."

	^ contextScope lookupVar: name inContext: aContext