Extension { #name : 'CompiledCode' }

{ #category : '*DebugInfo' }
CompiledCode >> debugInfo [

	^ OnlineSourceDebugInfo for: self
]
