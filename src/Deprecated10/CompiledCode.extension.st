Extension { #name : #CompiledCode }

{ #category : #'*Deprecated10' }
CompiledCode >> debuggerMap [
	self deprecated: 'please use the reflective API directly to read variables!'.
	^self compilerClass debuggerMethodMapForMethod: self.
]
