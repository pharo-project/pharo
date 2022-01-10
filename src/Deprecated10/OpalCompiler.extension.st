Extension { #name : #OpalCompiler }

{ #category : #'*Deprecated10' }
OpalCompiler class >> debuggerMethodMapForMethod: aMethod [
	self deprecated: 'please use the reflective API directly to read variables!'.
	^ DebuggerMethodMapOpal forMethod: aMethod
]
