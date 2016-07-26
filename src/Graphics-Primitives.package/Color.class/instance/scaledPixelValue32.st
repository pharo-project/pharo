conversions
scaledPixelValue32
	"Return the alpha scaled pixel value for depth 32"
	| pv32 a b g r |

	pv32 := self pixelWordForDepth: 32 .
	a := (self alpha * 255.0) rounded.
	b := (pv32 bitAnd: 255) * a // 256.
	g := ((pv32 bitShift: -8) bitAnd: 255) * a // 256.
	r := ((pv32 bitShift: -16) bitAnd: 255) * a // 256.
	^ b + (g bitShift: 8) + (r bitShift: 16) + (a bitShift: 24)