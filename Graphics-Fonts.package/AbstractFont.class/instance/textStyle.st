accessing
textStyle
	^ TextStyle actualTextStyles detect:
		[:aStyle | aStyle fontArray includes: self] ifNone: [ TextStyle fontArray: { self } ]