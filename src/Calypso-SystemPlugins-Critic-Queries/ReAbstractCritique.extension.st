Extension { #name : #ReAbstractCritique }

{ #category : #'*Calypso-SystemPlugins-Critic-Queries' }
ReAbstractCritique class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	| critique |
	critique := aBrowserItem actualObject.
	
	aBrowserItem addProperty: (ClyCritiqueSeverityProperty of: critique).
	critique sourceAnchor ifNotNil: [ :anchor | 
		anchor providesInterval ifTrue: [ 
			aBrowserItem addProperty: (ClySourceNodeCritiqueIntervalProperty from: anchor interval)]].
	critique providesChange	ifTrue: [ aBrowserItem markWith: ClyCritiqueFixableTag ]
]
