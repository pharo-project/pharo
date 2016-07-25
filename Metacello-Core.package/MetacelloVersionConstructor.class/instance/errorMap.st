accessing
errorMap
    errorMap ifNil: [ errorMap := Dictionary new ].
    ^ errorMap