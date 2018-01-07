Extension { #name : #TClass }

{ #category : #'*Ring2-Compatibility' }
TClass >> asRing2Definition [
	^self explicitRequirement.
]
