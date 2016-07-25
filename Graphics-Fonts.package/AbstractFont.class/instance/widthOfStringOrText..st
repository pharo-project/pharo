measuring
widthOfStringOrText: aStringOrText
    aStringOrText ifNil: [^0].
    ^aStringOrText isText
        ifTrue:[self approxWidthOfText: aStringOrText ]
        ifFalse:[self widthOfString: aStringOrText ] 