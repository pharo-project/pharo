pixel mapping
pixelMap: pixelValue 
	"Perform a reverse pixel mapping operation"
	| pv |
	colors == nil 
		ifTrue: [ pv := pixelValue ]
		ifFalse: [ pv := colors at: pixelValue ].
	(shifts == nil and: [ masks == nil ]) ifFalse: 
		[ pv := (((pv bitAnd: self redMask) bitShift: self redShift) bitOr: ((pv bitAnd: self greenMask) bitShift: self greenShift)) bitOr: (((pv bitAnd: self blueMask) bitShift: self blueShift) bitOr: ((pv bitAnd: self alphaMask) bitShift: self alphaShift)) ].
	"Need to check for translucency else Form>>paint goes gaga"
	pv = 0 ifTrue: [ pixelValue = 0 ifFalse: [ pv := 1 ] ].
	^ pv