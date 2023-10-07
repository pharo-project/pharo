Extension { #name : 'CompiledMethod' }

{ #category : '*Calypso-SystemPlugins-InheritanceAnalysis-Queries' }
CompiledMethod >> isOverriding [

	^ self overriddenMethod isNotNil
]
