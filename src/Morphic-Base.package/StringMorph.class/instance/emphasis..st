accessing
emphasis: aCodeOrTextEmphasis
	"Set my emphasis to aCodeOrTextEmphasis, which is either a TextEmphasis object,
	or a numeric code that is treated as a bitmask.   In the case of a TextEmphasis,
	sending the message value will return the code number.  
	
	The bits have the following significance (see also the class comment for TextEmphasis)
	
	bit 			attribute
	2r1	 (1) 		bold 
	2r10 (2)	 	italic 
	2r100 (4) 	underlined 
	2r1000	 (8) 	narrow 
	2r10000	 (16) 	strikethrough 
			
	examples: 
		2r0 -> plain.  
		2r1 -> bold. 
		2r11 -> bold + italic.  
		2r1101 -> bold + underlined + strikethrough.
	etc...
	"

	emphasis := aCodeOrTextEmphasis value.
	^ self font: font emphasis: emphasis