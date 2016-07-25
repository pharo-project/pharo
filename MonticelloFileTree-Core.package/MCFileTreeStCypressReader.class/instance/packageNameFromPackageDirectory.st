accessing
packageNameFromPackageDirectory
    | filename |
    filename := self fileUtils directoryName: packageDirectory.
    ^ filename copyFrom: 1 to: (filename lastIndexOf: $.) - 1