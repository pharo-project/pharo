Extension { #name : #InstructionStream }

{ #category : #'*Deprecated90' }
InstructionStream >> debuggerMap [
	self deprecated: 'use the reflective API directly. See methods of DebuggerMethodMapOpal'.
	^self method debuggerMap
]
