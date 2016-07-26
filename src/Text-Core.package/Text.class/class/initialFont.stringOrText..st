instance creation
initialFont: aStrikeFont stringOrText: aStringOrText
	"Answer an instance of me whose characters are aString."
	
	^Text string: aStringOrText asString attribute: (TextFontReference toFont: aStrikeFont)