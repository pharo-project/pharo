actions
selectParentDirectory
	"Switch to the parent directory."

	self hasParentDirectory ifFalse: [^self].
	self selectDirectory: self selectedFileDirectory asFileReference parent