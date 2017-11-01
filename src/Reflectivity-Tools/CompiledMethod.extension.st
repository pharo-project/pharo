Extension { #name : #CompiledMethod }

{ #category : #'*Reflectivity-Tools' }
CompiledMethod >> hasBreakpoint [
	^ Breakpoint isInstalledIn: self
	
	
]
