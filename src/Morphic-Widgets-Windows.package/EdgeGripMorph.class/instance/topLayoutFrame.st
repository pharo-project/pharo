as yet unclassified
topLayoutFrame
	"Answer the layout frame for a top edge."
	
	^ (0 @ 0 corner: 1 @ 0) asLayoutFrame 
		topLeftOffset: 22 @ -29 ;
		bottomRightOffset:  -22 @ (SystemWindow borderWidth - 29)