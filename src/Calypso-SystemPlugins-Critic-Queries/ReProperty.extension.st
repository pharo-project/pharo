Extension { #name : #ReProperty }

{ #category : #'*Calypso-SystemPlugins-Critic-Queries' }
ReProperty class >> convertToCalypsoBrowserItem: aProperty [
	^ClyBrowserItem named: aProperty title with: aProperty
]

{ #category : #'*Calypso-SystemPlugins-Critic-Queries' }
ReProperty class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	"Following code is to support Pharo 7 Exception property when there is problem with critiques.
	Pharo 6 has not this class. So this special trick is used"
	| property |
	property := aBrowserItem actualObject.
	property class name = #ReExceptionProperty ifTrue: [
		aBrowserItem markWith: ClyBrokenCritiqueTag]
]
