parsing
splitBadTokensIn: aString
	"split tokens such as BoldOblique, that should be two words"
	| str |
	str := aString.
	#(	('bold' 'oblique') ('bold' 'italic')
	) do:[:pair | | i |
		(i := str asLowercase findString: pair first, pair second startingAt: 1) > 0
			ifTrue:[
				str := (str first: i + pair first size - 1), ' ', (str last: (str size - (i + pair first size - 1)))]].
	^str