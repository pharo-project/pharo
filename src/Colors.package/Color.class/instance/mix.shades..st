groups of shades
mix: color2 shades: thisMany 
	"Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.  "
	"(Color red mix: Color green shades: 12) inspect"
	| redInc greenInc blueInc out rr gg bb |
	thisMany = 1 ifTrue: [ ^ Array with: color2 ].
	redInc := (color2 red - self red) / (thisMany - 1).
	greenInc := (color2 green - self green) / (thisMany - 1).
	blueInc := (color2 blue - self blue) / (thisMany - 1).
	rr := self red.
	gg := self green.
	bb := self blue.
	out := (1 to: thisMany) collect: 
		[ :num | | c | 
		c := self class 
			r: rr
			g: gg
			b: bb.
		rr := rr + redInc.
		gg := gg + greenInc.
		bb := bb + blueInc.
		c ].
	out 
		at: out size
		put: color2.	"hide roundoff errors"
	^ out