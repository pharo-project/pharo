utilities
writeStreamFor: filePath in: aDirectory do: aBlock
    (aDirectory resolveString: filePath) writeStreamDo: aBlock