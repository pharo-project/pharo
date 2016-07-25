as yet unclassified
rightLayoutFrame
	"Answer the layout frame for a right edge."
	
	^ (1 @ 0 corner: 1 @ 1) asLayoutFrame 
		topLeftOffset: SystemWindow borderWidth negated @ -7 ;
		bottomOffset: SystemWindow borderWidth - 26