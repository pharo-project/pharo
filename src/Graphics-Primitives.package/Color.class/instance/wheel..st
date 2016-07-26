groups of shades
wheel: thisMany 
	"An array of thisMany colors around the color wheel starting at self and ending all the way around the hue space just before self.  Array is of length thisMany.  Very useful for displaying color based on a variable in your program.  "
	| sat bri step hue |
	sat := self saturation.
	bri := self brightness.
	hue := self hue.
	step := 360.0 / (thisMany max: 1).
	^ (1 to: thisMany) collect: 
		[ :num | | c | 
		c := self class 
			h: hue
			s: sat
			v: bri.	"hue is taken mod 360"
		hue := hue + step.
		c ]
	"
(Color wheel: 8) withIndexDo: [:c :i | Display fill: (i*10@20 extent: 10@20) fillColor: c]
"