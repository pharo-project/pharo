conversion
translateTo: aTimeZone
	"Keep myself's representation and move it to another timezone offset.
	Note that unlike #offset: this WILL change the absolute time in UTC"
	
	^ self class
		starting: (start translateTo: aTimeZone)