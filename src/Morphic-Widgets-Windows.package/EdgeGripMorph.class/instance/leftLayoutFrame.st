as yet unclassified
leftLayoutFrame
	"Answer the layout frame for a left edge."
	
	^ (0 @ 0 corner: 0 @ 1) asLayoutFrame 
		topOffset: -7; 
		bottomRightOffset: SystemWindow borderWidth @ (SystemWindow borderWidth - 26)