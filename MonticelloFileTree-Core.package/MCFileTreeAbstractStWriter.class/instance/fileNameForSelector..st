private
fileNameForSelector: selector
    ^ (selector == #'/'
        ifTrue: [ 'encoded slash' ]
        ifFalse: [ 
            (selector includes: $/)
                ifTrue: [ 'encoded' , selector copyReplaceAll: '/' with: ' slash ' ]
                ifFalse: [ selector ] ]) asString