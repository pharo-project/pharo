utilities
parseMember: fileName
    | directory tokens |
    directory := self fileUtils directoryFromPath: self monticelloMetaDirName relativeTo: packageDirectory.
    self fileUtils readStreamFor: fileName in: directory do: [ :fileStream | tokens := self scanner scan: fileStream ].
    ^ self associate: tokens