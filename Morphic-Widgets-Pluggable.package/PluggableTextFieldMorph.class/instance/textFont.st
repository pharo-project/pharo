accessing
textFont
	"We should clean"
	
	^ textFont ifNil: [ StandardFonts defaultFont  ]