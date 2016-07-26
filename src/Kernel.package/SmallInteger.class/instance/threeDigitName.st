printing
threeDigitName

	| units answer |

	self = 0 ifTrue: [^''].
	units := #('one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' 'ten' 
		'eleven' 'twelve' 'thirteen' 'fourteen' 'fifteen' 'sixteen' 'seventeen' 
		'eighteen' 'nineteen').
	self > 99 ifTrue: [
		answer := (units at: self // 100),' hundred'.
		(self \\ 100) = 0 ifFalse: [
			answer := answer,' ',(self \\ 100) threeDigitName
		].
		^answer
	].
	self < 20 ifTrue: [
		^units at: self
	].
	answer := #('twenty' 'thirty' 'forty' 'fifty' 'sixty' 'seventy' 'eighty' 'ninety')
			at: self // 10 - 1.
	(self \\ 10) = 0 ifFalse: [
		answer := answer,'-',(units at: self \\ 10)
	].
	^answer