accessing
description
    "Return a textual description of the exception."

    ^ 'Could not resolve: ' , packageSpec name , ' [' , packageSpec file , ']' , ' in' , self repositoryString
        ,
            (repositoryError == nil
                ifTrue: [ '' ]
                ifFalse: [ 
                    "report repository error to user here, since failure here is likely to be due to earlier repository error"
                    ' ERROR: ' , repositoryError description printString ])