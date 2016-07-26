accessing
errorMap: anObject
    errorMap ifNil: [ errorMap := Dictionary new ].
    errorMap := anObject