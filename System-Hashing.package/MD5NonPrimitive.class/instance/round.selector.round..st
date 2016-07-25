private-rounds
round: data selector: selector round: round 
	"Do one round with the given function"
	| shiftIndex template abcd |
	1 
		to: 16
		do: 
			[ :i | 
			shiftIndex := (i - 1) \\ 4 + 1.
			abcd := ABCDTable at: shiftIndex.
			template := { 
				(abcd at: 1).
				(abcd at: 2).
				(abcd at: 3).
				(abcd at: 4).
				((IndexTable at: round) at: i).
				((ShiftTable at: round) at: shiftIndex).
				(SinTable at: (round - 1) * 16 + i)
			 }.
			self 
				step: data
				template: template
				selector: selector ]