cleanup
removeFrom: aNode
	| links breakpointsToRemove |
	links := aNode beforeLinks select: [ :link | link metaObject = Break].
	breakpointsToRemove := AllBreakpoints select: [ :br | links includes: br link].
	breakpointsToRemove do: #remove.