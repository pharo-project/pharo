attributes
unembellished 
	"Return true if the only emphases are the default font and bold"
	| font1 bold |
	font1 := TextFontChange defaultFontChange.
	bold := TextEmphasis bold.
	runs withStartStopAndValueDo:
		[:start :stop :emphArray |
		emphArray do:
			[:emph | (font1 = emph or: [bold = emph]) ifFalse: [^ false]]].
	^ true