Extension { #name : #ClyBrowserContext }

{ #category : #'*Calypso-SystemPlugins-Spotter' }
ClyBrowserContext >> asGoToSpotterCandidate [
	^ClyGoToSpotterCandidate 
		target: self activeQueryView
		named: self simpleNameForSpotterMenu
		icon: self iconForSpotterMenu
		using: [ self activeQueryView takeKeyboardFocus ]
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
ClyBrowserContext >> iconForSpotterMenu [
	^self iconNameForSpotterMenu ifNotNil: [ :iconName |
		Smalltalk ui iconNamed: iconName]
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
ClyBrowserContext >> iconNameForSpotterMenu [
	^nil
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
ClyBrowserContext >> simpleNameForSpotterMenu [
	^self class name
]
