class initialization
initialize
	"MD5 initialize"
	"Obscure fact: those magic hex numbers that are hard to type in correctly are
	actually the result of a simple trigonometric function and are therefore
	easier to compute than proofread.  Laziness is sometimes a virtue."
	| c |
	c := 2 raisedTo: 32.
	SinTable := Array new: 64.
	1 
		to: 64
		do: 
			[ :i | 
			SinTable 
				at: i
				put: (ThirtyTwoBitRegister new load: (c * i sin abs) truncated) ].
	ShiftTable := { 
		#(7 12 17 22 ).
		#(5 9 14 20 ).
		#(4 11 16 23 ).
		#(6 10 15 21 )
	 }.
	IndexTable := { 
		#(
			1
			2
			3
			4
			5
			6
			7
			8
			9
			10
			11
			12
			13
			14
			15
			16
		).
		#(
			2
			7
			12
			1
			6
			11
			16
			5
			10
			15
			4
			9
			14
			3
			8
			13
		).
		#(
			6
			9
			12
			15
			2
			5
			8
			11
			14
			1
			4
			7
			10
			13
			16
			3
		).
		#(
			1
			8
			15
			6
			13
			4
			11
			2
			9
			16
			7
			14
			5
			12
			3
			10
		)
	 }.
	ABCDTable := { 
		#(1 2 3 4 ).
		#(4 1 2 3 ).
		#(3 4 1 2 ).
		#(2 3 4 1 )
	 }