initialization
specificTableMorphInitializiation
	"Intialize first the columns and only then do the rest of the initialization."
	self specificTableMorphColumnsInitialization.
	super specificTableMorphInitializiation.
	