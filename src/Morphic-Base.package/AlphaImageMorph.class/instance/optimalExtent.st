geometry
optimalExtent
	"Answer the optimal extent for the receiver."
	
	^self form extent * self scale + (self borderWidth * 2)