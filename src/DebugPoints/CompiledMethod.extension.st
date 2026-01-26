Extension { #name : 'CompiledMethod' }

{ #category : '*DebugPoints' }
CompiledMethod >> hasBreakpoint [

	^ (DebugPoint allInstalledInMethod: self) notEmpty
]
