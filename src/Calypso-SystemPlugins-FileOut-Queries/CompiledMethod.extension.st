Extension { #name : #CompiledMethod }

{ #category : #'*Calypso-SystemPlugins-FileOut-Queries' }
CompiledMethod >> fileOut [

	^self origin fileOutMethod: self selector
]
