as yet unclassified
renderOn: aCanvas

	aCanvas setPaint: color.
	aCanvas drawShape: (originX @ originY extent: extent).
"	children do: [ :each |
		each renderOn: aCanvas
	].
	[  ^ self ] value.
	"
	children do: [ :each | | path |
		
		"we cache the path connecting each child and its parent, to not recreate it each time"
		path := aCanvas cacheAt: each ifAbsentPut: [ 
			| midTop midBot |
			
			midTop := self midBottom.
			midBot := each midTop.
			
			aCanvas createPath: [:builder | 
				builder 
					absolute;
					moveTo: self midBottom;
					curveVia: midTop x@midBot y and: midBot x@ midTop y to: midBot
					"lineTo: each midTop"
			]].

		aCanvas setStrokePaint: (Color red alpha:0.5).
		aCanvas drawShape: path. 
		
		each renderOn: aCanvas
	]