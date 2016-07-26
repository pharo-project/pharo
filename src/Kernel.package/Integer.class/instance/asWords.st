printing
asWords
	"SmallInteger maxVal asWords"
	| mils minus three num answer milCount |
	self = 0 ifTrue: [^'zero'].
	mils := #('' ' thousand' ' million' ' billion' ' trillion' ' quadrillion' ' quintillion' ' sextillion' ' septillion' ' octillion' ' nonillion' ' decillion' ' undecillion' ' duodecillion' ' tredecillion' ' quattuordecillion' ' quindecillion' ' sexdecillion' ' septendecillion' ' octodecillion' ' novemdecillion' ' vigintillion').
	num := self.
	minus := ''.
	self < 0 ifTrue: [
		minus := 'negative '.
		num := num negated.
	].
	answer := String new.
	milCount := 1.
	[num > 0] whileTrue: [
		three := (num \\ 1000) threeDigitName.
		num := num // 1000.
		three isEmpty ifFalse: [
			answer isEmpty ifFalse: [
				answer := ', ',answer
			].
			answer := three,(mils at: milCount),answer.
		].
		milCount := milCount + 1.
	].
	^minus,answer