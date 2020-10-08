Extension { #name : #OpalCompiler }

{ #category : #'*Deprecated90' }
OpalCompiler class >> debuggerMethodMapForMethod: aMethod [
	self deprecated: 'use the reflective API directly. See methods of DebuggerMethodMapOpal'.
	^ DebuggerMethodMapOpal forMethod: aMethod
]
