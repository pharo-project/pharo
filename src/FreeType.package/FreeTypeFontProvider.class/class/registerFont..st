extra fonts registration
registerFont: aClassWithFont

	"Do nothing if freetype is disabled currently"
	
	FreeTypeSystemSettings loadFt2Library ifTrue: [ 
		aClassWithFont installFontsIn: self current
		 ]