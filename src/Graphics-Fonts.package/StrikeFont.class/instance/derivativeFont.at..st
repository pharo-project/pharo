emphasis
derivativeFont: aStrikeFont at: index

	| newDeriv |
	(aStrikeFont isNil and: [ index = 0 ]) 
		ifTrue: [derivativeFonts := nil. ^ self].
	derivativeFonts ifNil: [derivativeFonts := Array new: 32].
	derivativeFonts size < 32 ifTrue: [
		newDeriv := Array new: 32.
		newDeriv replaceFrom: 1 to: derivativeFonts size with: derivativeFonts.
		derivativeFonts := newDeriv.
	].
	derivativeFonts at: index put: aStrikeFont.