filling
fillFromYColorBlock: colorBlock
	"Vertical Gradient Fill.
	Supply relative y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| yRel |
	0 to: height-1 do:
		[:y |  yRel := y asFloat / (height-1) asFloat.
		self fill: (0@y extent: width@1) 
			fillColor: (colorBlock value: yRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromYColorBlock: [:y | Color r: y g: 0.0 b: 0.5]) display
"