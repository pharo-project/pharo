Extension { #name : #ClyInheritedMethodGroup }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClyInheritedMethodGroup >> addTraitVisibilityLevels [

	| traitLevels |
	visibilityLevels size >= 2 ifTrue: [ 
		"Workaround of problem when groups are do not represented as browser query result.
		In that case cached group instance receives this message multiple tymes by trait plugin.
		Normal solution should move levels to browser item as properties"
		^self ].
	
	traitLevels := visibilityLevels 
		collect: [:each | each asTraitVisibility] thenReject: [:each | each isRedundant].
		
	visibilityLevels := visibilityLevels , traitLevels
]
