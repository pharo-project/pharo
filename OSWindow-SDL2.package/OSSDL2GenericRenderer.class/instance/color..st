rendering
color: aColor
	| red green blue alpha |
	red := self mapColorChannel: aColor red.
	green := self mapColorChannel: aColor green.
	blue := self mapColorChannel: aColor blue.
	alpha := self mapColorChannel: aColor alpha.
	renderer drawColorR: red g: green b: blue a: alpha