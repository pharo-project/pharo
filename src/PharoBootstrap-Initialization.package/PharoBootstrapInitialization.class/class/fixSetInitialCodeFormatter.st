temporary-patch
fixSetInitialCodeFormatter
	"We need to install a formatter object.
	If not, source code cannot be formatted 
		=> AST from existing methods cannot be built
		=> errors cannot be displayed
		..."
	RBMethodNode formatterClass: (Smalltalk globals at: #BISimpleFormatter).