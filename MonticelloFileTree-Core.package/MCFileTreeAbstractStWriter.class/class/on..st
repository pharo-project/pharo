writing
on: aStream
    MCFileTreePackageStructureStWriter useCypressWriter
        ifTrue: [ ^ MCFileTreeStCypressWriter new stream: aStream ].
    ^ (MCFileTreePackageStructureStWriter useSnapShotWriter
        ifTrue: [ MCFileTreeStSnapshotWriter ]
        ifFalse: [ MCFileTreeStWriter ]) new stream: aStream