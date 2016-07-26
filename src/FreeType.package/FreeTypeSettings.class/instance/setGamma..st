accessing
setGamma: aFloat

	(aFloat closeTo: self gamma)
		ifFalse:[
			gamma := aFloat.
			(gamma closeTo: 1.0)
				ifTrue:[gammaTable := gammaInverseTable := nil]
				ifFalse:[
					gammaTable := ByteArray new: 256.
					gammaInverseTable := ByteArray new: 256.
					0 to: 255 do:[:i |
						| g ug |
						g := ((i / 255.0) raisedTo: (1.0/gamma)) * 255.
						ug := ((i / 255.0) raisedTo: gamma) * 255.
						g := (g rounded min: 255) max: 0 .
						ug := (ug rounded min: 255) max: 0 .
						gammaTable at: i + 1 put: g.
						gammaInverseTable at: i + 1 put: ug]].
				World restoreMorphicDisplay]