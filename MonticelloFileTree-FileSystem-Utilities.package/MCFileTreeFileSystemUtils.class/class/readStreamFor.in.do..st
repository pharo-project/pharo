utilities
readStreamFor: filePath in: aDirectory do: aBlock
    "temporary hack for Pharo-1.4"

    ^ (aDirectory resolveString: filePath)
        readStreamDo: [ :fileStream | 
            | stream |
            stream := ReadStream on: fileStream contents asString.
            stream reset.
            aBlock value: stream ]