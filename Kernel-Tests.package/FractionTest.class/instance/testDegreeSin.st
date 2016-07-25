tests - mathematical functions
testDegreeSin
	"self run: #testDegreeSin"

	(4 / 3) degreeSin.
	-361 / 3 to: 359 / 3 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ]