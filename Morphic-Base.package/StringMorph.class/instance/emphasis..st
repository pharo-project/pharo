font
emphasis: aNumber
	"
	Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format:
	
	bit 			attribute
	2r1	 (1) 		bold 
	2r10 (2)	 	italic 
	2r100 (4) 	underlined 
	2r1000	 (8) 	strikethrough 
			
	examples: 
		2r0 -> plain.  
		2r1 -> bold. 
		2r11 -> bold + italic.  
		2r1101 -> bold + underlined + strikethrough.
	etc...
	"

	emphasis := aNumber.
	
	^ self font: font emphasis: emphasis