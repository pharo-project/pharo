accessing
defaultWorkingDirectory
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."
	| pathString |

	pathString := Primitives decode: Primitives imageFile.
	^(Path from: pathString delimiter: self delimiter) parent