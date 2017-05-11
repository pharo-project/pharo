*UnifiedFFI
signedByteAt: byteOffset
    "Return a 8bit signed integer starting at the given byte offset"
    ^self integerAt: byteOffset size: 1 signed: true