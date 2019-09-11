Extension { #name : #ClyBrowserToolMorph }

{ #category : #'*Calypso-SystemPlugins-Spotter' }
ClyBrowserToolMorph >> asGoToSpotterCandidate [

	^ClyGoToSpotterCandidate 
		target: self
		named: self simpleNameForSpotterMenu
		icon: self defaultIcon
		using: [ self selectAsMainTab; takeKeyboardFocus ]
]

{ #category : #'*Calypso-SystemPlugins-Spotter' }
ClyBrowserToolMorph >> simpleNameForSpotterMenu [

	^self defaultTitle
]
