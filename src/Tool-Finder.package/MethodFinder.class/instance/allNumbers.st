find a constant
allNumbers
	"Return true if all answers and all data are numbers."

	answers do: [:aa | aa isNumber ifFalse: [^ false]].
	thisData do: [:vec |
			vec do: [:nn | nn isNumber ifFalse: [^ false]]].
	^ true