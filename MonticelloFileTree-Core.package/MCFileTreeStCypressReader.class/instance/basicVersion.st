accessing
basicVersion
    self hasMonticelloMetadata
        ifTrue: [ ^ super basicVersion ].
    ^ MCVersion new
        setPackage: self package
            info: self info
            snapshot: self snapshot
            dependencies: #();
        yourself