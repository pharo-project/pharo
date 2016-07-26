as yet unclassified
"protocol: testing"
appliesToDebugger: aDebugger

	^ (aDebugger session isContextPostMortem: aDebugger currentContext) not and: [ 
		aDebugger bytecodePresentation selection offset > aDebugger currentContext pc ]