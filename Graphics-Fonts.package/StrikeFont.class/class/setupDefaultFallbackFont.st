accessing
setupDefaultFallbackFont
	"
	StrikeFont setupDefaultFallbackFont
	"
	(self defaultFallbackFontKeys
		collect: [:e | TextStyle named: e])
		do: [:style | style fontArray
				do: [:e | 
					e reset.
					(e respondsTo: #fontArray)
						ifTrue: [e fontArray 
								do: [:f | f setupDefaultFallbackFont]]]].
