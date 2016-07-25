converting
orderedDither32To16
	"Do an ordered dithering for converting from 32 to 16 bit depth."
	| ditherMatrix ii out inBits outBits index pv dmv r di dmi dmo g b pvOut outIndex |
	self depth = 32 ifFalse:[^self error:'Must be 32bit for this'].
	ditherMatrix := #(	0	8	2	10
						12	4	14	6
						3	11	1	9
						15	7	13	5).
	ii := (0 to: 31) collect:[:i| i].
	out := Form extent: self extent depth: 16.
	inBits := self bits.
	outBits := out bits.
	index := outIndex := 0.
	pvOut := 0.
	0 to: self height-1 do:[:y|
		0 to: self width-1 do:[:x|
			pv := inBits at: (index := index + 1).
			dmv := ditherMatrix at: (y bitAnd: 3) * 4 + (x bitAnd: 3) + 1.
			r := pv bitAnd: 255.	di := r * 496 bitShift: -8.
			dmi := di bitAnd: 15.	dmo := di bitShift: -4.
			r := dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			g := (pv bitShift: -8) bitAnd: 255.	di := g * 496 bitShift: -8.
			dmi := di bitAnd: 15.	dmo := di bitShift: -4.
			g := dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			b := (pv bitShift: -16) bitAnd: 255.	di := b * 496 bitShift: -8.
			dmi := di bitAnd: 15.	dmo := di bitShift: -4.
			b := dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			pvOut := (pvOut bitShift: 16) + 
						(b bitShift: 10) + (g bitShift: 5) + r.
			(x bitAnd: 1) = 1 ifTrue:[
				outBits at: (outIndex := outIndex+1) put: pvOut.
				pvOut := 0].
		].
		(self width bitAnd: 1) = 1 ifTrue:[
			outBits at: (outIndex := outIndex+1) put: (pvOut bitShift: -16).
			pvOut := 0].
	].
	^out