font accessing
haloFont
	^ HaloFont ifNil: [HaloFont := StandardFonts defaultFont]