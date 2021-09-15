Extension { #name : #InstructionStream }

{ #category : #'*Deprecated10' }
InstructionStream >> debuggerMap [
	self deprecated: 'please use the reflective API directly to read variables!'.
	^self method debuggerMap
]
