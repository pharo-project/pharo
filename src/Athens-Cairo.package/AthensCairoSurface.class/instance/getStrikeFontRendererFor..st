text support
getStrikeFontRendererFor: aStrikeFont 
	"answer the same instance, just reset it's font and advance"
	
	^ AthensStrikeFontRenderer on: context  forFont: aStrikeFont.