actions
selectDirectory: aFileReference
	"Expand and select the given directory."

	self changed: #(openPath), aFileReference pathSegments