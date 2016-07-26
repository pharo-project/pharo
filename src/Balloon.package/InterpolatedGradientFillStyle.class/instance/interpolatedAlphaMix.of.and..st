as yet unclassified
interpolatedAlphaMix: ratio of: rgba1 and: rgba2
	"Answer a proper interpolated value between two RGBA color words.
	Theta is 0..1.."

	| a1 a2 ra ira rgb1 rgb2 alpha br1 br2 bg1 bg2 bb1 bb2 result |
	a1 := rgba1 bitShift: -24. a2 := rgba2 bitShift: -24.
	alpha := ratio * (a2 - a1) + a1.
	ra := ratio * alpha.
	ira := (1.0 - ratio) * alpha.
	rgb1 := rgba1 bitAnd: 16rFFFFFF. rgb2 := rgba2 bitAnd: 16rFFFFFF.
	br1 := (rgb1 bitAnd: 255). br2 := (rgb2 bitAnd: 255).
	bg1 := ((rgb1 bitShift:  -8) bitAnd: 255). bg2 := ((rgb2 bitShift: -8) bitAnd: 255).
	bb1 := ((rgb1 bitShift: -16) bitAnd: 255). bb2 := ((rgb2 bitShift: -16) bitAnd: 255).
	result :=  (ra * br2 + (ira * br1)) rounded // 255.
	result :=  result bitOr: ((ra * bg2 + (ira * bg1)) rounded // 255 bitShift: 8).
	result :=  result bitOr: ((ra * bb2 + (ira * bb1)) rounded // 255 bitShift: 16).
	^result bitOr: (alpha rounded bitShift: 24)