Extension { #name : #ClyFullVisibilityLevel }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClyFullVisibilityLevel >> asTraitVisibility [
	^ClyLocalTraitsVisibilityLevel of: visibleClassScope
]
