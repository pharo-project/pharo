accessing
atAllPut: anObject
       "Put anObject at every one of the receiver's indices."

       1 to: self basicSize do:
               [:i|
               self basicAt: i put: nil].
       defaultValue := anObject