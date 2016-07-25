accessing
sort: aSortBlock 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."
 
 	super sort: aSortBlock.
 	sortBlock := aSortBlock