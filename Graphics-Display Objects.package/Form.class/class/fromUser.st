instance creation
fromUser
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen designated by the user. The grid for selecting an area is 
	1@1."
	"self fromUser asMorph openInWorld"
	
	^self fromUser: 1 @ 1