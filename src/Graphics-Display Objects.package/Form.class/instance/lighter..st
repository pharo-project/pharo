converting
lighter: aFactor
	"Answer a lighter variant of this form. aFactor is a float from 0 to 1 representing the strength of 
	the lightening."
	"(Form fromUser lighter: 0.16) asMorph openInWorld"
	"(Form fromUser lighter: 0.30) asMorph openInWorld"
	^self collectColors: [:color | 
			color adjustSaturation: -0.03 brightness: aFactor]