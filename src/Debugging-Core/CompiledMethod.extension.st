Extension { #name : #CompiledMethod }

{ #category : #'*Debugging-Core' }
CompiledMethod >> debuggerMap [
	^self compilerClass debuggerMethodMapForMethod: self.
]
