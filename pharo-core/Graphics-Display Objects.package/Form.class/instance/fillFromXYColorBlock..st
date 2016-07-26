filling
fillFromXYColorBlock: colorBlock
	"General Gradient Fill.
	Supply relative x and y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| poker yRel xRel |
	poker := BitBlt bitPokerToForm: self.
	0 to: height-1 do:
		[:y | yRel := y asFloat / (height-1) asFloat.
		0 to: width-1 do:
			[:x |  xRel := x asFloat / (width-1) asFloat.
			poker pixelAt: x@y
				put: ((colorBlock value: xRel value: yRel) pixelWordForDepth: self depth)]]
"
 | d |
((Form extent: 100@20 depth: Display depth)
	fillFromXYColorBlock:
	[:x :y | d := 1.0 - (x - 0.5) abs - (y - 0.5) abs.
	Color r: d g: 0 b: 1.0-d]) display
"