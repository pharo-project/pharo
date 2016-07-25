converting
asScaledDecimal: scale 
	"Answer the receiver converted to a ScaledDecimal."
	
	^ ScaledDecimal newFromNumber: self scale: scale