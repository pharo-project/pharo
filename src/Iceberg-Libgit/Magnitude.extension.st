Extension { #name : #Magnitude }

{ #category : #'*Iceberg-Libgit' }
Magnitude >> iceSemanticVersion [
	^ IceSemanticVersion major: self asInteger minor: 0 patch: 0
]
