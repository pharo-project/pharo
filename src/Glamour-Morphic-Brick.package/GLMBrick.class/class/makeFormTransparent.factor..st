utils
makeFormTransparent: aForm factor: aFloat
	"a float must be in interval [0.0, 1.0]"
	| a |
	
	aForm ifNil: [ ^ nil ].
	aForm depth = 32 ifFalse: [^ aForm ].
	
	a := (aFloat * 255) asInteger.
	1 to: aForm bits size do: [:i |
		| v r g b |
		v := aForm bits at: i.
		r := ((v bitShift: -16) bitAnd: 255) * a // 255.
		g := ((v bitShift: -8) bitAnd: 255) * a // 255.
		b := (v bitAnd: 255) * a // 255.
		aForm bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b ].
	^ aForm 