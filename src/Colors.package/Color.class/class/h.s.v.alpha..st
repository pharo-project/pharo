instance creation
h: hue s: saturation v: brightness alpha: alpha
	^ self basicNew
		initializeHue: hue saturation: saturation brightness: brightness alpha: alpha ;
		yourself 
	