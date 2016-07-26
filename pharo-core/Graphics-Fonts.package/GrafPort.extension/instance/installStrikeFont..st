*Graphics-Fonts
installStrikeFont: aStrikeFont

	^ self installStrikeFont: aStrikeFont foregroundColor: (lastFontForegroundColor ifNil: [Color black]) backgroundColor: (lastFontBackgroundColor ifNil: [Color transparent]).
