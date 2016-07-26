accessing
nextPut: aString
    "Append the argument, aString, to the receiver, doubling embedded terminators."
 
    | i remainder terminator | 
    terminator := $!.
    remainder := aString asString.
    [(i := remainder indexOf: terminator) = 0] whileFalse:
        [decoratedStream nextPutAll: (remainder copyFrom: 1 to: i).
        decoratedStream nextPut: terminator.  "double imbedded terminators"
        remainder := remainder copyFrom: i+1 to: remainder size].
    decoratedStream nextPutAll: remainder. 
    aString asString includesUnifiedCharacter ifTrue: [
        decoratedStream nextPut: terminator.
        decoratedStream nextPutAll: ']lang['.
        aString asString writeLeadingCharRunsOn: decoratedStream.
    ].
    decoratedStream nextPut: terminator.
    decoratedStream flush.