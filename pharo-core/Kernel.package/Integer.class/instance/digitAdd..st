private
digitAdd: arg 
	| len arglen accum sum |
	<primitive: 'primDigitAdd' module:'LargeIntegers'>
	accum := 0.
	(len := self digitLength) < (arglen := arg digitLength) ifTrue: [len := arglen].
	"Open code max: for speed"
	sum := Integer new: len neg: self negative.
	1 to: len do: 
		[:i | 
		accum := (accum bitShift: -8)
					+ (self digitAt: i) + (arg digitAt: i).
		sum digitAt: i put: (accum bitAnd: 255)].
	accum > 255
		ifTrue: 
			[sum := sum growby: 1.
			sum at: sum digitLength put: (accum bitShift: -8)].
	^ sum