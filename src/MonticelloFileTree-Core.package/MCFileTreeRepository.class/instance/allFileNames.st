accessing
allFileNames
    ^ (self directory entries select: [ :entry | entry isDirectory and: [ self canReadFileNamed: entry name ] ])
        collect: [ :entry | entry name ]