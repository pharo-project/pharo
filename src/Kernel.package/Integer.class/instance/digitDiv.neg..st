private
digitDiv: arg neg: ng 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ (ZeroDivide dividend: self) signal].
	"TFEI added this line"
	l := self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d := 8 - arg lastDigit highBitOfPositiveReceiver.
	div := arg digitLshift: d.
	div := div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem := self digitLshift: d.
	rem digitLength = self digitLength ifTrue: [rem := rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo := Integer new: l neg: ng.
	dl := div digitLength - 1.
	"Last actual byte of data"
	ql := l.
	dh := div digitAt: dl.
	dnh := dl = 1
				ifTrue: [0]
				ifFalse: [div digitAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j := rem digitLength + 1 - k.
		"r1 := rem digitAt: j."
		(rem digitAt: j)
			= dh
			ifTrue: [qhi := qlo := 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 := (rem digitAt: j - 1)."
				t := ((rem digitAt: j)
							bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitShift: -4).
				qhi := t // dh.
				t := (t \\ dh bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitAnd: 15).
				qlo := t // dh.
				t := t \\ dh.
				"Next compute (hi,lo) := q*dnh"
				hi := qhi * dnh.
				lo := qlo * dnh + ((hi bitAnd: 15)
								bitShift: 4).
				hi := (hi bitShift: -4)
							+ (lo bitShift: -8).
				lo := lo bitAnd: 255.
				"Correct overestimate of q.  
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 := j < 3
							ifTrue: [0]
							ifFalse: [rem digitAt: j - 2].
				[(t < hi
					or: [t = hi and: [r3 < lo]])
					and: 
						["i.e. (t,r3) < (hi,lo)"
						qlo := qlo - 1.
						lo := lo - dnh.
						lo < 0
							ifTrue: 
								[hi := hi - 1.
								lo := lo + 256].
						hi >= dh]]
					whileTrue: [hi := hi - dh].
				qlo < 0
					ifTrue: 
						[qhi := qhi - 1.
						qlo := qlo + 16]].
		"Subtract q*div from rem"
		l := j - dl.
		a := 0.
		1 to: div digitLength do: 
			[:i | 
			hi := (div digitAt: i)
						* qhi.
			lo := a + (rem digitAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div digitAt: i)
							* qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a := lo // 256 - (hi bitShift: -4).
			l := l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo := qlo - 1.
				l := j - dl.
				a := 0.
				1 to: div digitLength do: 
					[:i | 
					a := (a bitShift: -8)
								+ (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l := l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem := rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem