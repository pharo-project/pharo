utilities
directoryFromPath: directoryPath
    ^ (AbsolutePath from: directoryPath delimiter: self pathNameDelimiter) asFileReference