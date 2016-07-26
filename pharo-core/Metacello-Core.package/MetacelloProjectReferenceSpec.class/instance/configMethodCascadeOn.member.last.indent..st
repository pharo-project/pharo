printing
configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent
    aMember methodUpdateSelector == #'remove:'
        ifTrue: [ aStream nextPutAll: 'removeProject: ' , self name printString ]
        ifFalse: [ 
            self projectReference == nil
                ifTrue: [ ^ self ].
            aStream nextPutAll: self projectLabel , ': ' , self projectName printString , ' '.
            (aMember methodUpdateSelector == #'copy:' and: [ self projectReference hasNonVersionStringField ])
                ifTrue: [ aStream nextPutAll: 'copyFrom: ' , aMember sourceName printString , ' ' ].
            self projectReference configShortCutMethodOn: aStream member: aMember indent: indent + 1 ].
    lastCascade
        ifTrue: [ aStream nextPut: $. ]
        ifFalse: [ 
            aStream
                nextPut: $;;
                cr ]