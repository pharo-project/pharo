filling
fill: aRectangle rule: anInteger fillColor: aForm 
	"Replace a rectangular area of the receiver with the pattern described by aForm 
	according to the rule anInteger."
	(BitBlt toForm: self)
		copy: aRectangle
		from: 0@0 in: nil
		fillColor: aForm rule: anInteger