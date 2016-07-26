drawing
loadOn: aCanvas
	fontFamily notNil ifTrue:[
		self
			primSelectFont:  fontFamily
				slant:  fontSlant
				weight:  fontWeight 
				on: aCanvas] .
	fontSize notNil ifTrue:[		
		self primSetFontSize:  (TextStyle pointsToPixels: fontSize) on: aCanvas ]