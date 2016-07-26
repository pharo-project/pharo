actions
resetContentsRecursively 
	self resetContents.
	self allContents do: [:each | each resetContentsRecursively]