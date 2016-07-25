removing
removeAll
	"See super."
	
	| tmp |
	tmp := keyBlock.
	super removeAll.
	keyBlock := tmp