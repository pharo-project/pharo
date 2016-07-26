accessing
target: aTarget
	target := aTarget.
	morph := target morph.
	"self announcer weak
		on: MorphLostFocus send: #clearBuffer to: KMBuffer uniqueInstance."