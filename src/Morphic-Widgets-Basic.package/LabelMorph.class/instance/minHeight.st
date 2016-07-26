layout
minHeight
	"Answer the receiver's minimum height.
	based on font height."
	
	^self fontToUse height rounded max: super minHeight