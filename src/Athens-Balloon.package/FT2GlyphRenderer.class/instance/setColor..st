accessing
setColor: color
	"Set the color which will be used to render glyphs."
	
	| clr rgb alpha |
	currentColor = color ifTrue: [ ^ self ].
	currentColor := color.
	clr := self pixelValue32Of: color. 
	rgb := clr bitAnd: 16rFFFFFF.
	alpha := clr >> 24.
	
 	0 to: 255 do:[:i | | a |
		a :=  (i+1) * alpha bitAnd: 16rFF00.
		colorMap at: i+1 put: ( (a<<16) + rgb ).
	].

"	colorMap at: 1 put: (Color red  alpha: 0.1) pixelValue32."
