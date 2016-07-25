accessing
iconPosition
	"Position of the icon. 
	  #left - Icon will be left to the label
	  #right - Icon will be right to the label 
	  #top - Icon will be top to the label"
	^ iconPosition ifNil: [ iconPosition := #left ]