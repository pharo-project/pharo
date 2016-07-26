other
preMultiplyAlpha
	"Pre-multiply each pixel by its alpha, for proper alpha compositing (BitBlt rule 34).
	E.g., half-transparent green 16r7F00FF00 becomes 16r7F007F00"

	depth = 32 ifFalse: [^self].
	1 to: bits size do: [:i |
		| v a r g b |
		v := bits at: i.
		a := v bitShift: -24.
		r := ((v bitShift: -16) bitAnd: 255) * a // 255.
		g := ((v bitShift: -8) bitAnd: 255) * a // 255.
		b := (v bitAnd: 255) * a // 255.
		bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b].