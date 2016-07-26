utilities
filePathExists: filePath relativeTo: aDirectory
    ^ (aDirectory resolveString: filePath) isFile