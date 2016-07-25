as yet unclassified
bottomLayoutFrame
	"Answer the layout frame for a bottom edge."
	
	^ (0 @ 1 corner: 1 @ 1) asLayoutFrame 
		topLeftOffset: 22 @ SystemWindow borderWidth negated ;
		rightOffset: -22