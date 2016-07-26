accessing
sizeBuffer: size
    buffer ifNotNil: [self flushBuffer].
    buffer := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: size
