utility
areLowercaseDigitsAllowed
	"Answer true if lowercase letter are allowed digits."
	
	^(NumberParser parse: '16re' onError: [-1]) = 16rE