private
digitMultiply: arg neg: ng 
	| prod prodLen carry digit k ab |
	<primitive: 'primDigitMultiplyNegative' module:'LargeIntegers'>
	(arg digitLength = 1 and: [(arg digitAt: 1)
			= 0])
		ifTrue: [^ 0].
	(self digitLength = 1 and: [(self digitAt: 1)
			= 0])
		ifTrue: [^ 0].
	prodLen := self digitLength + arg digitLength.
	prod := Integer new: prodLen neg: ng.
	"prod starts out all zero"
	1 to: self digitLength do: [:i | (digit := self digitAt: i) ~= 0
			ifTrue: 
				[k := i.
				carry := 0.
				"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg digitLength do: 
					[:j | 
					ab := (arg digitAt: j)
								* digit + carry + (prod digitAt: k).
					carry := ab bitShift: -8.
					prod digitAt: k put: (ab bitAnd: 255).
					k := k + 1].
				prod digitAt: k put: carry]].
	^ prod normalize