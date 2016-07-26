public
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock
	"Copy the file referenced as sourcePath to the destination referred as destPath. Perform associate actions in case of problems."

	store
		copy: (self resolve: sourcePath)
		ifAbsent: absentBlock
		to: (self resolve: destinationPath)
		ifPresent: presentBlock
		fileSystem: self