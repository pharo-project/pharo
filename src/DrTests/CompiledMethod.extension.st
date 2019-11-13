Extension { #name : #CompiledMethod }

{ #category : #'*DrTests' }
CompiledMethod >> asResultForDrTest [
	^ DTTestLeaf content: self
]
