Extension { #name : #PharoBootstrapRule }

{ #category : #'*Deprecated10' }
PharoBootstrapRule >> collectionsPackages [
	self deprecated: 'Deprecated together with assembly mechanism. See issue #9670'.
	
	^ (CollectionsAssembly parts collect: [:part| part key])
		reject: [ :each | each = #'Collections-Atomic' ]
]
