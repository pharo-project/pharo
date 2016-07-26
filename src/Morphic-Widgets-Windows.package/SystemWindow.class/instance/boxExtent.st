initialization
boxExtent
	"answer the extent to use in all the buttons.  
	The label height is used to be proportional to the standard window label font"
	
	label ifNil: [^14 @ 14].
	^ (14 @ 14) max: label height @ label height 