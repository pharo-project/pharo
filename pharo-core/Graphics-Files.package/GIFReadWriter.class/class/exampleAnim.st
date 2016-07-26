examples
exampleAnim
	"GIFReadWriter exampleAnim"

	| writer extent center |
	writer := GIFReadWriter on: (FileStream newFileNamed: 'anim.gif').
	writer loopCount: 20.		"Repeat 20 times"
	writer delay: 10.		"Wait 10/100 seconds"
	extent := 42@42.
	center := extent / 2.
	Cursor write showWhile: [
		[2 to: center x - 1 by: 2 do: [:r |
			"Make a fancy anim without using Canvas - inefficient as hell"
			| image |
			image := ColorForm extent: extent depth: 8.
			0.0 to: 359.0 do: [:theta | image colorAt: (center + (Point r: r degrees: theta)) rounded put: Color red].
			writer nextPutImage: image]
		]	ensure: [writer close]].