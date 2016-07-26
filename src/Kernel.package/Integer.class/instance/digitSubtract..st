private
digitSubtract: arg 
	| smaller larger z sum sl al ng |
	<primitive: 'primDigitSubtract' module:'LargeIntegers'>
	sl := self digitLength.
	al := arg digitLength.
	(sl = al
		ifTrue: 
			[[(self digitAt: sl)
				= (arg digitAt: sl) and: [sl > 1]]
				whileTrue: [sl := sl - 1].
			al := sl.
			(self digitAt: sl)
				< (arg digitAt: sl)]
		ifFalse: [sl < al])
		ifTrue: 
			[larger := arg.
			smaller := self.
			ng := self negative == false.
			sl := al]
		ifFalse: 
			[larger := self.
			smaller := arg.
			ng := self negative].
	sum := Integer new: sl neg: ng.
	z := 0.
	"Loop invariant is -1<=z<=1"
	1 to: sl do: 
		[:i | 
		z := z + (larger digitAt: i) - (smaller digitAt: i).
		sum digitAt: i put: z - (z // 256 * 256).
		"sign-tolerant form of (z bitAnd: 255)"
		z := z // 256].
	^ sum normalize